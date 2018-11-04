module Main where

import           Control.Monad
import           Data.Default
import           StdDraw
import           StdDraw.Colors

-- test = do
---  square 0.2 0.8 0.1
---  filledSquare 0.8 0.8 0.2
---   circle 0.8 0.2 0.2
--
--   setPenColor bookRed
--   setPenRadius 0.02
--   arc 0.8 0.2 0.1 200 45
--
--   // draw a blue diamond
--   setPenRadius
--   setPenColor bookBlue
---  let x = [ 0.1, 0.2, 0.3, 0.2 ]
---      y = [ 0.2, 0.3, 0.2, 0.1 ]
---  filledPolygon x y
--
--   setPenColor black
--   text 0.2 0.5 "black text"
--   setPenColor white
--   text 0.8 0.8 "white text"

main :: IO ()
main = withWindow "Hello, world!" conf draw

conf :: DrawConfig
conf = def
  { defaultClearColor = green
  }

draw :: DrawApp ()
draw = do
  forever $ do
    clearDefault

    square 0.2 0.8 0.1
    filledSquare 0.8 0.8 0.2
    circle 0.8 0.2 0.2

    let x = [ 0.1, 0.2, 0.3, 0.2 ]
        y = [ 0.2, 0.3, 0.2, 0.1 ]
    filledPolygon x y

    showBuffer

