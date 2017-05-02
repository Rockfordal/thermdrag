module Components.Container where

import Components.Router as Router
import WebSocket as WS
import Components.Config (wsUrl)
import Control.Coroutine (Consumer, Producer, consumer, runProcess, ($$))
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=))
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Nothing))
import Halogen (action, liftEff)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Prelude (type (~>), Unit, ($), discard, pure, bind, unit)


-- Producer coroutine emits messages that arrives from websocket.
wsProducer :: forall e . WS.Connection
                      -> Producer String (Aff (avar :: AVAR, err :: EXCEPTION
                                             , ajax :: AJAX, ws  :: WS.WEBSOCKET | e)) Unit
wsProducer (WS.Connection socket) =
  produce \emit -> do
      socket.onmessage $= \event -> do
        emit $ Left $ WS.runMessage (WS.runMessageEvent event)


-- Consumer coroutine takes the `query` function from our component IO
-- record and sends `AddMessage` queries in when it receives inputs from producer
wsConsumer :: forall e. (Router.Input ~> Aff (HalogenEffects e))
                     -> Consumer String (Aff (HalogenEffects e)) Unit
wsConsumer query =
  consumer \msg -> do
    query $ action $ Router.IncomingMessage msg
    pure Nothing


-- Consumer coroutine takes output messages from our component IO, sends with websocket
wsSender :: forall e. WS.Connection -> Consumer Router.Output
                      (Aff (HalogenEffects (ws :: WS.WEBSOCKET, ajax :: AJAX
                                         , err :: EXCEPTION | e))) Unit
wsSender (WS.Connection socket) = consumer \msg -> do
  case msg of
    Router.OutputMessage msgContents -> do
        text <- pure $ msgContents
        liftEff $ socket.send (WS.Message text)
  pure Nothing


containerapp :: Eff (HalogenEffects (ws :: WS.WEBSOCKET, ajax :: AJAX
                                  , err :: EXCEPTION)) Unit
containerapp = do
  conn <- WS.newWebSocket (WS.URL wsUrl) []
  runHalogenAff do
    el <- awaitBody
    io <- runUI Router.ui unit el
    _  <- forkAff $ Router.routeSignal io

    -- wsSender subscribes to our components output messages
    io.subscribe $ wsSender conn

    -- We feed queries back to our component
    -- Connectiong consumer to producer
    -- Both will be initialized
    runProcess (wsProducer conn $$ wsConsumer io.query)

