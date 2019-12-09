import Data.Char (digitToInt, isDigit)
import Data.List (sortBy)
import Data.List.Split (chunksOf)

type Layer = [Int]

toLayers :: Int -> Int -> [Int] -> [Layer]
toLayers width height = chunksOf (width * height)

countDigit :: Int -> [Int] -> Int
countDigit d = length . filter (== d)

main :: IO ()
main =
  getContents >>=
  (\input ->
     let layers = toLayers 25 6 $ map digitToInt $ filter isDigit input
         minZeros =
           head $
           sortBy (\a b -> compare (countDigit 0 a) (countDigit 0 b)) layers
         checkSum = (countDigit 1 minZeros) * (countDigit 2 minZeros)
      in putStrLn $ show checkSum)
