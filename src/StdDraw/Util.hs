module StdDraw.Util where

import           Data.Fixed                (mod')
import qualified Graphics.Rendering.OpenGL as GL

degToRad :: Double -> Double
degToRad = (pi / 180 *)

mod2pi :: Double -> Double
mod2pi = flip mod' (pi * 2)

verticesUnsafe :: [Double] -> [Double] -> [GL.Vertex3 GL.GLfloat]
verticesUnsafe xs ys
  | length xs == length ys = fmap vertex3 $ zip xs ys
  | otherwise = error "Lists must have the same length"

vertex3 :: (Double, Double) -> GL.Vertex3 GL.GLfloat
vertex3 (x, y) = GL.Vertex3 (realToFrac x) (realToFrac y) 0

ellipseVertices :: Double -> Double -> Double -> Double -> [(Double, Double)]
ellipseVertices x y w h = fmap circleVertex [0..299]
  where
    circleVertex i = let angle = 2 * pi * i / 300
                      in (x + w * (cos angle), y + h * (sin angle))

