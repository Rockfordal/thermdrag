module Components.Container where

import Prelude
import Components.Log as Log
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Halogen as H
import Halogen.Aff as HA
import WebSocket as WS
import Components.Log (Message)
import Control.Coroutine (Await)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=))
import Control.Monad.Free.Trans (FreeT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (liftEff)
import Halogen.VDom.Driver (runUI)
import WebSocket (Connection, WEBSOCKET)


-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer
  :: forall eff
   . WS.Connection
  -> CR.Producer String (Aff (avar :: AVAR, err :: EXCEPTION, ws :: WS.WEBSOCKET | eff)) Unit
wsProducer (WS.Connection socket) = CRA.produce \emit -> do
  socket.onmessage $= \event -> do
    emit $ Left $ WS.runMessage (WS.runMessageEvent event)

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `AddMessage` queries in when it receives inputs from the
-- producer.
wsConsumer
  :: forall eff
   . (Log.Query ~> Aff (HA.HalogenEffects eff))
  -> CR.Consumer String (Aff (HA.HalogenEffects eff)) Unit
wsConsumer query = CR.consumer \msg -> do
  query $ H.action $ Log.AddMessage msg
  pure Nothing

-- A consumer coroutine that takes output messages from our component IO
-- and sends them using the websocket
-- wsSender
--   :: forall eff
--    . WS.Connection
--   -> CR.Consumer Log.Message (Aff (HA.HalogenEffects (ws :: WS.WEBSOCKET | eff))) Unit    
wsSender :: forall eff t3 t4
    . Monad t4 => MonadEff
                  ( ws :: WEBSOCKET
                  , err :: EXCEPTION
                  | eff)
                  t4 => Connection -> FreeT (Await Message) t4 t3
wsSender (WS.Connection socket) = CR.consumer \msg -> do
  case msg of
    Log.OutputMessage msgContents ->
      liftEff $ socket.send (WS.Message msgContents)
  pure Nothing


containerapp :: Eff (HA.HalogenEffects (ws :: WS.WEBSOCKET, err :: EXCEPTION)) Unit
containerapp = do
--   let url = "ws://echo.websocket.org"
--   let url = "ws://localhost:4000"
  let url = "ws://46.162.127.62:4500"
  connection <- WS.newWebSocket (WS.URL url) []
  HA.runHalogenAff do
    body <- HA.awaitBody
    io <- runUI Log.component unit body

    -- The wsSender consumer subscribes to all output messages from our component
    io.subscribe $ wsSender connection
        
    -- Connecting the consumer to the producer initializes both,
    -- feeding queries back to our component as messages are received.
    CR.runProcess (wsProducer connection CR.$$ wsConsumer io.query)

