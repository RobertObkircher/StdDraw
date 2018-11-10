module Main where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Default
import           StdDraw
import           StdDraw.Colors
import           System.IO

main :: IO ()
main = withWindow "Hello, world!" conf draw

conf :: DrawConfig
conf = def
  { defaultClearColor = green
  }

draw :: DrawApp ()
draw = do
  liftIO $ hSetBuffering stdout LineBuffering
  forever $ do
    clearDefault
    setDefaultPenColor

    test

    showBuffer

test :: DrawApp ()
test = do
  square 0.2 0.8 0.1
  filledSquare 0.8 0.8 0.2
  circle 0.8 0.2 0.2

  setPenColor bookRed
  setPenRadius 0.02
  arc 0.8 0.2 0.1 200 45

  setDefaultPenRadius
  setPenColor bookBlue
  let x = [ 0.1, 0.2, 0.3, 0.2 ]
      y = [ 0.2, 0.3, 0.2, 0.1 ]
  filledPolygon x y

--   setPenColor black
--   text 0.2 0.5 "black text"
--   setPenColor white
--   text 0.8 0.8 "white text"
