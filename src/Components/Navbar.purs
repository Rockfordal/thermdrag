module Components.Navbar where

-- import Prelude
-- import Data.Array as A
-- import Halogen as H
-- import Halogen.HTML (div_)
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
-- import Control.Monad.Aff (Aff)

-- type State =
--   { dummy :: Boolean
--   }

-- data Query a
--   = UpdateInputText String a

-- data Message
--   = OutputMessage String                 

-- -- component :: forall eff. H.Component HH.HTML Query String Message (Aff (ajax :: AJAX | eff))
-- component :: forall eff. H.Component HH.HTML Query String Message eff
-- component =
--   H.component
--     { initialState: const initialState
--     , render
--     , eval
--     , receiver: HE.input UpdateInputText
--     }
--   where
--   initialState :: State
--   initialState = { dummy: false }

--   render :: State -> H.ComponentHTML Query
--   render state =
--     div_
--       [ div_ []
--       ]

-- -- <nav class="navbar navbar-inverse navbar-fixed-top">
-- --       <div class="container">
-- --         <div class="navbar-header">
-- --           <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
-- --             <span class="sr-only">Toggle navigation</span>
-- --             <span class="icon-bar"></span>
-- --             <span class="icon-bar"></span>
-- --             <span class="icon-bar"></span>
-- --           </button>
-- --           <a class="navbar-brand" href="#">Project name</a>
-- --         </div>
-- --         <div id="navbar" class="navbar-collapse collapse">
-- --           <form class="navbar-form navbar-right">
-- --             <div class="form-group">
-- --               <input type="text" placeholder="Email" class="form-control" style="background-image: url(&quot;data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAASCAYAAABSO15qAAAAAXNSR0IArs4c6QAAAPhJREFUOBHlU70KgzAQPlMhEvoQTg6OPoOjT+JWOnRqkUKHgqWP4OQbOPokTk6OTkVULNSLVc62oJmbIdzd95NcuGjX2/3YVI/Ts+t0WLE2ut5xsQ0O+90F6UxFjAI8qNcEGONia08e6MNONYwCS7EQAizLmtGUDEzTBNd1fxsYhjEBnHPQNG3KKTYV34F8ec/zwHEciOMYyrIE3/ehKAqIoggo9inGXKmFXwbyBkmSQJqmUNe15IRhCG3byphitm1/eUzDM4qR0TTNjEixGdAnSi3keS5vSk2UDKqqgizLqB4YzvassiKhGtZ/jDMtLOnHz7TE+yf8BaDZXA509yeBAAAAAElFTkSuQmCC&quot;); background-repeat: no-repeat; background-attachment: scroll; background-size: 16px 18px; background-position: 98% 50%;">
-- --             </div>
-- --             <div class="form-group">
-- --               <input type="password" placeholder="Password" class="form-control" style="background-image: url(&quot;data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAASCAYAAABSO15qAAAAAXNSR0IArs4c6QAAAPhJREFUOBHlU70KgzAQPlMhEvoQTg6OPoOjT+JWOnRqkUKHgqWP4OQbOPokTk6OTkVULNSLVc62oJmbIdzd95NcuGjX2/3YVI/Ts+t0WLE2ut5xsQ0O+90F6UxFjAI8qNcEGONia08e6MNONYwCS7EQAizLmtGUDEzTBNd1fxsYhjEBnHPQNG3KKTYV34F8ec/zwHEciOMYyrIE3/ehKAqIoggo9inGXKmFXwbyBkmSQJqmUNe15IRhCG3byphitm1/eUzDM4qR0TTNjEixGdAnSi3keS5vSk2UDKqqgizLqB4YzvassiKhGtZ/jDMtLOnHz7TE+yf8BaDZXA509yeBAAAAAElFTkSuQmCC&quot;); background-repeat: no-repeat; background-attachment: scroll; background-size: 16px 18px; background-position: 98% 50%;">
-- --             </div>
-- --             <button type="submit" class="btn btn-success">Sign in</button>

--   eval :: Query ~> H.ComponentDSL State Query Message (Aff (ajax :: AJAX | eff))
-- --   eval :: Query ~> H.ComponentDSL State Query Message eff
--   eval (UpdateInputText text next) = do
--     H.modify (_ { inputText = text })
--     pure next
