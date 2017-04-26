module Components.Typer where
-- import Data.Maybe (Maybe)

type State = Boolean

data Query a
  = Toggle a
  | IsOn (Boolean -> a)

data Message = Toggled Boolean


-- type State =
--   { name :: String
-- --   , drag :: Maybe Drag
-- --   , circles :: Array Circle
--   }

-- type Connection = 
--   { from :: Int
--   , to :: Int
--   }

-- type Circle =
--   { r :: Int
--   , x :: Int
--   , y :: Int
--   , c :: String
--   }

-- type Drag =
--   { start :: Position
--   , current :: Position
--   , index :: Int
--   }

-- type Position =
--   { x :: Int
--   , y :: Int
--   }

-- data Action
--   = KABOOM
--   | DragStart Int Position
--   | DragAt Position
--   | DragEnd