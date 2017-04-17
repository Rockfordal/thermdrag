module Components.Typer where
import Data.Maybe (Maybe)


type State =
  { name :: String
  , drag :: Maybe Drag
  , circles :: Array Circle
  -- , connections :: Array Connection
  }

type Connection = 
  { from :: Int
  , to :: Int
  }

type Circle =
  { r :: Int
  , x :: Int
  , y :: Int
  , c :: String
  }

type Drag =
  { start :: Position
  , current :: Position
  , index :: Int
  }

type Position =
  { x :: Int
  , y :: Int
  }

data Action
  = Rubbe
  | DragStart Int Position
  | DragAt Position
  | DragEnd


-- patches
--   nodes      
--     eval ""
--     connections
--       inputs [("numbera", Nothing),
--               ("numberb", Nothing)]
--       outputs [("resultat", Nothing)]