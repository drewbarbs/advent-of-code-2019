import Data.Char (digitToInt, intToDigit, isDigit)
import Data.List.Split (chunksOf)

type Layer = [Int]

toLayers :: Int -> Int -> [Int] -> [Layer]
toLayers width height = chunksOf (width * height)

combinePixels :: Int -> Int -> Int
combinePixels 2 p = p
combinePixels p _ = p

replace :: Eq a => a -> a -> [a] -> [a]
replace a b =
  map
    (\c ->
       if c == a
         then b
         else c)

main :: IO ()
main =
  getContents >>=
  (\input ->
     let layers = toLayers 25 6 $ map digitToInt $ filter isDigit input
         pxValues = foldl1 (zipWith combinePixels) layers
         img =
           foldr (\a b -> a ++ "\n" ++ b) "" $
           chunksOf 25 $ replace '0' ' ' $ map intToDigit pxValues
      in putStr img)
