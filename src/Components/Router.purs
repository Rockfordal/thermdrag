module Components.Router where
import Prelude
import Components.Login as Login
import Components.Navbar as Navbar
import Components.Chat as Chat
import Components.Profile as Profile
import Components.Sessions as Sessions
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.State.Class (modify)
import Data.Either.Nested (Either5)
import Data.Functor.Coproduct (Coproduct)
import Data.Functor.Coproduct.Nested (Coproduct5)
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Data.Tuple (Tuple(..))
import Halogen (Component)
import Halogen.Component.ChildPath (ChildPath, cp1, cp2, cp3, cp4, cp5)
import Halogen.HTML (HTML, a, div_, h1_, li_, slot', text, ul_)
import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (lit, num)

data Input a
  = Goto Routes a
  | IncomingMessage String a
  | HandleNavbar Navbar.Message a
  | HandleLogin Login.Message a
  | HandleChat Chat.Message a

data CRUD
  = Index
  | Show Number

data Routes
  = Profile
  | Sessions CRUD
  | Home

data Message
  = OutputMessage String                 

init :: State
init = { currentPage: "Home" }

routing :: Match Routes
routing = profile
      <|> sessions
      <|> home
  where
    profile = Profile <$ route "profile"
    home = Home <$ lit ""
    -- chat = Home <$ lit ""
    sessions = Sessions <$> (route "sessions" *> parseCRUD)
    route str = lit "" *> lit str
    parseCRUD = Show <$> num <|> pure Index

type State =
  { currentPage :: String
  }

type QueryP
  = Coproduct Input ChildQuery

type ChildQuery = Coproduct5 Profile.Input Sessions.Input Navbar.Input Login.Input Chat.Input
type ChildSlot  = Either5    Profile.Slot  Sessions.Slot  Navbar.Slot  Login.Slot  Chat.Slot

pathToProfile :: ChildPath Profile.Input ChildQuery Profile.Slot ChildSlot
pathToProfile = cp1

pathToSessions :: ChildPath Sessions.Input ChildQuery Sessions.Slot ChildSlot
pathToSessions = cp2

pathToNavbar :: ChildPath Navbar.Input ChildQuery Navbar.Slot ChildSlot
pathToNavbar = cp3

pathToLogin :: ChildPath Login.Input ChildQuery Login.Slot ChildSlot
pathToLogin = cp4

pathToChat :: ChildPath Chat.Input ChildQuery Chat.Slot ChildSlot
pathToChat = cp5



-- ui :: forall m. Component HTML Input Unit Message m
ui :: forall eff. Component HTML Input Unit Message (Login.LoginEff eff)
ui = H.parentComponent
  { initialState: const init
  , render
  , eval
  , receiver: const Nothing
  }
  where
  render :: State -> H.ParentHTML Input ChildQuery ChildSlot (Login.LoginEff eff)
  render st =
    div_
      [ slot' pathToNavbar Navbar.Slot Navbar.ui unit (HE.input HandleNavbar)
      , slot' pathToLogin  Login.Slot  Login.ui  unit (HE.input HandleLogin)
      , slot' pathToChat   Chat.Slot   Chat.ui   ""   (HE.input HandleChat)
      , h1_ [ text (st.currentPage) ]
      , ul_ (map link ["Sessions", "Profile", "Home"])
      , viewPage st.currentPage
      ]

  link s = li_ [ a [ HP.href ("#/" <> toLower s) ] [ text s ] ]

  viewPage :: String -> H.ParentHTML Input ChildQuery ChildSlot (Login.LoginEff eff)
  viewPage "Sessions" =
    slot' pathToSessions Sessions.Slot Sessions.ui unit absurd
  viewPage "Profile" =
    slot' pathToProfile Profile.Slot Profile.ui unit absurd
  viewPage _ =
    div_ [ text "Du är hemma"]

  eval :: Input ~> H.ParentDSL State Input ChildQuery ChildSlot Message (Login.LoginEff eff)
  eval (Goto Profile next) = do
    modify (_ { currentPage = "Profile2" })
    pure next
  eval (Goto (Sessions view) next) = do
    modify case view of
      Index  -> (_ { currentPage = "Sessions" })
      Show n -> (_ { currentPage = "Session " <> show n })
    pure next
  eval (Goto Home next) = do
    H.raise $ OutputMessage "We went somewhere!"
    modify (_ { currentPage = "Home" })
    pure next
  eval (IncomingMessage text next) = do
    let msgtext = "Received: " <> text
    _ <- H.query' pathToChat Chat.Slot (H.request (Chat.AddMessage msgtext))
    pure next
  eval (HandleNavbar (Navbar.SetPage page) next) = do
    pure next
  eval (HandleLogin (Login.GotToken token) next) = do
    H.raise $ OutputMessage token
    pure next
  eval (HandleChat (Chat.OutputMessage msg) next) = do
    H.raise $ OutputMessage msg
    pure next

routeSignal :: forall eff. H.HalogenIO Input Message (Aff (HA.HalogenEffects eff))
            -> Aff (HA.HalogenEffects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

redirects :: forall eff. H.HalogenIO Input Message (Aff (HA.HalogenEffects eff))
          -> Maybe Routes
          -> Routes
          -> Aff (HA.HalogenEffects eff) Unit
redirects driver _ =
  driver.query <<< H.action <<< Goto
