module Components.Base where
-- import Prelude
-- import Components.Navbar as Navbar
-- import Halogen as H
-- import Halogen.HTML.Events as HE
-- import Control.Monad.Aff (Aff)
-- import Data.Either.Nested (Either2)
-- import Data.Functor.Coproduct.Nested (Coproduct2)
-- import Data.Maybe (Maybe(Nothing))
-- import Halogen.Component.ChildPath (cp1, cp2, cp3)
-- import Halogen.HTML (HTML, div_, slot')
-- import Halogen.HTML.Events (input_)
-- import Network.HTTP.Affjax (AJAX)

-- type State =
--   { dummy :: Int }

-- data Query a
--   = IncomingMessage String a
--   | HandleNavbar Navbar.Message a
--   | HandleRutt Rutt.Message a
--   -- | HandleLogin Login.Message a
--   -- | HandleChat Chat.Message a

-- data Message
--   = OutputMessage String                 

-- -- type ChildQuery = Coproduct3 Navbar.Query Login.Query Chat.Query
-- -- type ChildSlot  = Either3 Unit Unit Unit 

-- type ChildQuery = Coproduct2 Navbar.Query Rutt.Input
-- type ChildSlot  = Either2 Unit Unit

-- type BaseEff eff = Aff (ajax :: AJAX | eff)

-- component :: forall eff. H.Component HTML Query Unit Message (BaseEff eff)
-- component =
--   H.parentComponent
--     { initialState: const initialState
--     , render
--     , eval
--     , receiver: const Nothing
--     }
--   where
--   initialState :: State
--   initialState = { dummy: 0 }

--   render :: State -> H.ParentHTML Query ChildQuery ChildSlot (BaseEff eff)
--   render state =
--     div_
--       [ slot' cp1 unit Navbar.ui unit (HE.input HandleNavbar)
--       , slot' cp2 unit Rutt.ui unit   (HE.input HandleRutt)
--       -- , slot' cp2 unit Login.component unit  (HE.input HandleLogin)
--       -- , slot' cp3 unit Chat.component  ""    (HE.input HandleChat)
--       ]

--   eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (BaseEff eff)
--   eval (HandleNavbar (Navbar.SetPage page) next) = do
--     pure next
--   eval (HandleRutt (Rutt.Rutta _) next) = do
--     pure next
--   -- eval (HandleLogin (Login.GotToken tokstr) next) = do
--   --   H.raise $ OutputMessage tokstr
--   --   pure next
--   -- eval (HandleChat (Chat.OutputMessage msg) next) = do
--   --   H.raise $ OutputMessage msg
--   --   pure next
--   eval (IncomingMessage text next) = do
--     let msgtext = "Received: " <> text
--     -- s <- H.query' cp3 unit (H.request (Chat.AddMessage msgtext))
--     pure next
