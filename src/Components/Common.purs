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
    [ unsafeMkProps "x1" x1
    , unsafeMkProps "y1" y1
    , unsafeMkProps "x2" x2
    , unsafeMkProps "y2" y2
    , unsafeMkProps "stroke" "black"
    , unsafeMkProps "stroke-width" 2 
    ] []