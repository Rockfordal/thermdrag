module Components.Common where

import React.DOM.SVG as S
import Thermite as T
import React (ReactElement)
import React.DOM.Props (unsafeMkProps)


rawview :: Array ReactElement -> T.Spec _ _ _ _
rawview h = T.simpleSpec T.defaultPerformAction render
  where
  render _ _ _ _ = h


linje :: Int -> Int -> Int -> Int -> ReactElement 
linje x1 y1 x2 y2 =
  S.line
    [ p "x1" x1
    , p "y1" y1
    , p "x2" x2
    , p "y2" y2
    , p "stroke" "black"
    , p "stroke-width" 2 
    ] []
    where p = unsafeMkProps