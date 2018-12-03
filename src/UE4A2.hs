module UE4A2(main) where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Default
import           Data.List
import           StdDraw
import           StdDraw.Colors
import           System.Random

genRandomArray :: Int -> Int -> IO [Int]
genRandomArray numValues maxValue = replicateM numValues $ randomRIO (0, maxValue)

getMaxValueFromArray :: [Int] -> Int
getMaxValueFromArray = maximum

calcStatistics :: [Int] -> [Int]
calcStatistics array = fmap countN $ [0..getMaxValueFromArray array]
  where
    countN n = length $ filter (==n) array

drawBarChart :: [Int] -> DrawApp ()
drawBarChart array = do
  setCanvasSize 600 400

  forM_ (zip [0..] array) $ \(i, value) -> do
    let x = (i + 0.5) / (fromIntegral $ length array)
    let y = (fromIntegral value) / (2 * max)
    setPenColor green
    filledRectangle x y halfBarWidth y
    setPenColor black
    rectangle x y halfBarWidth y
  showBuffer
  where
    halfBarWidth = 0.5 / (fromIntegral $ length array)
    max = fromIntegral $getMaxValueFromArray array

printArray :: [Int] -> DrawApp ()
printArray array = liftIO $ print array

main :: IO ()
main = withWindow "UE4A2" def $ do
  myArray <- liftIO $ genRandomArray 50 20
  let myStat = calcStatistics myArray
  printArray myArray
  printArray myStat
  drawBarChart myStat
  pause 3000

  myArray <- liftIO $ genRandomArray 100 10
  let myStat = calcStatistics myArray
  printArray myArray
  printArray myStat
  drawBarChart myStat
  pause 3000
