module Components.Navbar where

import Halogen.Themes.Bootstrap3 as B
import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Halogen (Component, component, modify)
import Halogen.Component (ComponentHTML, ComponentDSL)
import Halogen.HTML (HTML, a, button, div, div_, nav, text, ul)
import Halogen.HTML.Properties (class_, classes, target)
import Halogen.HTML.Properties.ARIA (controls, expanded)
import Prelude hiding (div)

type State =
  { dummy :: Boolean }

data Input a
  = UpdateInputText String a

data Message
  = SetPage String                 

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

ui :: forall eff. Component HTML Input Unit Message eff
ui =
  component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  initialState :: State
  initialState = { dummy: false }

  render :: State -> ComponentHTML Input
  render state =
    nav [ classes [ B.navbarNav, B.navbarFixedTop, B.navbarInverse] ]
      [ container_
        [ a [ classes [ B.navbarBrand ]
              -- , P.href (link Home)
              ]
          [ text "Yo" ]
        , ul [ classes [ B.navbarNav, B.nav, B.navTabs] ]
          [
          -- [ H.li_ [ linkTo (Sessions </> New) "Log" ]
          -- , H.li_ [ linkTo Profile "Profile" ]
          ]
        -- , case muser of
        --        Nothing ->
        --            H.ul [ P.classes [ B.nav, B.navbarNav, B.navTabs, B.navbarRight ] ]
        --                [ H.li_ [ linkTo Login "Log in" ]
        --                , H.li_ [ linkTo Registration "Sign up" ] ]
        --        Just u ->
        --            H.ul [ P.classes [ B.nav, B.navbarNav, B.navTabs, B.navbarRight ] ]
        --                [ H.li_ [ linkTo Logout "Log out" ] ]
        ]
      ]

  container attrs = div (class_ B.container : attrs)
  container_ = container []
  -- render state = div_
  --   [ div [ class_ (ClassName "navbar navbar-inverse navbar-fixed-top") ]
  --     [ div [ class_ (ClassName "container") ]
  --       [ div [ class_ (ClassName "navbar-header") ]
  --         [ button [ class_ (ClassName "navbar-toggle collapsed")
  --                  , expanded "false"
  --                  , controls "navbar"
  --                  , target "#navbar"
                  --  ] [] ] ] ] ]

-- <nav class="navbar navbar-inverse navbar-fixed-top">
--       <div class="container">
--         <div class="navbar-header">
--           <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
--             <span class="sr-only">Toggle navigation</span>
--             <span class="icon-bar"></span> <span class="icon-bar"></span> <span class="icon-bar"></span>
--           </button>
--           <a class="navbar-brand" href="#">Project name</a>
--         </div>
--         <div id="navbar" class="navbar-collapse collapse">
--           <form class="navbar-form navbar-right">
--             <div class="form-group">
--               <input type="text" placeholder="Email" class="form-control" style="background-image: url(&quot;data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAASCAYAAABSO15qAAAAAXNSR0IArs4c6QAAAPhJREFUOBHlU70KgzAQPlMhEvoQTg6OPoOjT+JWOnRqkUKHgqWP4OQbOPokTk6OTkVULNSLVc62oJmbIdzd95NcuGjX2/3YVI/Ts+t0WLE2ut5xsQ0O+90F6UxFjAI8qNcEGONia08e6MNONYwCS7EQAizLmtGUDEzTBNd1fxsYhjEBnHPQNG3KKTYV34F8ec/zwHEciOMYyrIE3/ehKAqIoggo9inGXKmFXwbyBkmSQJqmUNe15IRhCG3byphitm1/eUzDM4qR0TTNjEixGdAnSi3keS5vSk2UDKqqgizLqB4YzvassiKhGtZ/jDMtLOnHz7TE+yf8BaDZXA509yeBAAAAAElFTkSuQmCC&quot;); background-repeat: no-repeat; background-attachment: scroll; background-size: 16px 18px; background-position: 98% 50%;">
--             </div>
--             <div class="form-group">
--               <input type="password" placeholder="Password" class="form-control" style="background-image: url(&quot;data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAASCAYAAABSO15qAAAAAXNSR0IArs4c6QAAAPhJREFUOBHlU70KgzAQPlMhEvoQTg6OPoOjT+JWOnRqkUKHgqWP4OQbOPokTk6OTkVULNSLVc62oJmbIdzd95NcuGjX2/3YVI/Ts+t0WLE2ut5xsQ0O+90F6UxFjAI8qNcEGONia08e6MNONYwCS7EQAizLmtGUDEzTBNd1fxsYhjEBnHPQNG3KKTYV34F8ec/zwHEciOMYyrIE3/ehKAqIoggo9inGXKmFXwbyBkmSQJqmUNe15IRhCG3byphitm1/eUzDM4qR0TTNjEixGdAnSi3keS5vSk2UDKqqgizLqB4YzvassiKhGtZ/jDMtLOnHz7TE+yf8BaDZXA509yeBAAAAAElFTkSuQmCC&quot;); background-repeat: no-repeat; background-attachment: scroll; background-size: 16px 18px; background-position: 98% 50%;">
--             </div>
--             <button type="submit" class="btn btn-success">Sign in</button>

-- klass :: forall i r. String -> IProp ( "class" :: String | r) i
-- klass s = class_ (ClassName s)

  eval :: Input ~> ComponentDSL State Input Message eff
  eval (UpdateInputText text next) = do
    modify (_ { dummy = true })
    pure next