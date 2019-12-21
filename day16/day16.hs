import Control.DeepSeq (deepseq)
import Data.Char (digitToInt, intToDigit, isDigit)
import Data.List (concatMap, foldl')

basePattern :: [Int]
basePattern = [0, 1, 0, -1]

repeatElements :: [a] -> Int -> [a]
repeatElements xs n = concatMap (replicate n) xs

applyPhase :: [Int] -> [Int]
applyPhase digits = map calcDigit $ take (digits `deepseq` length digits) [0 ..]
  where
    calcDigit :: Int -> Int
    calcDigit 0 = go 0 1 0 digits 0
    calcDigit i = go i 0 0 digits 0
    go :: Int -> Int -> Int -> [Int] -> Int -> Int
    go i patOffset dIndex xs sumSoFar =
      if dIndex == length digits
        then sumSoFar
        else let nextOffset = (patOffset + 1) `mod` 4
                 mult = i + 1
                 (ys, zs) = splitAt mult xs
                 cont = go i nextOffset (dIndex + mult) zs
              in case (basePattern !! patOffset) of
                   0 -> cont sumSoFar
                   1 -> cont $ sumSoFar + (foldl' (+) 0 ys)
                   (-1) -> cont $ sumSoFar - (foldl' (+) 0 ys)

calcNPhases :: Int -> Int -> [Int] -> String
calcNPhases toDrop n digits =
  let resultDigits = head $ drop n $ take (n + 1) $ iterate applyPhase digits
   in map intToDigit $ take 8 $ drop toDrop resultDigits

part1 :: [Int] -> String
part1 = calcNPhases 0 100

part2 :: [Int] -> String
part2 inputList =
  let digitsToInt :: [Int] -> Int
      digitsToInt = sum . map (uncurry $ (*) . (10 ^)) . zip [0 ..] . reverse
      toSkip = digitsToInt $ take 7 inputList
      repeatedLen = length inputList * 10
   in calcNPhases toSkip 100 (take repeatedLen $ cycle inputList)

main :: IO ()
main = do
  input <- fmap (map digitToInt . filter isDigit) getContents
  putStrLn $ show (part1 input, part2 input)
