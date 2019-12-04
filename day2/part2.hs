set :: Int -> Int -> [Int] -> [Int]
set idx v xs = (take idx xs) ++ [v] ++ (drop (idx + 1) xs)

runProgram :: [Int] -> [Int]
runProgram prog = go prog prog 0
  where
    go :: [Int] -> [Int] -> Int -> [Int]
    go (99:xs) p _ = p
    go (opCode:in1:in2:out:_) p offset =
      go (drop nextOffset nextState) nextState nextOffset
      where
        op =
          if opCode == 1
            then (+)
            else (*)
        nextState = set out ((p !! in1) `op` (p !! in2)) p
        nextOffset = offset + 4

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn splitter = foldl f [[]]
  where
    f splits cur
      | cur == splitter = mappend splits [[]]
      | otherwise = init splits ++ [(last splits ++ [cur])]

pairToResult :: [Int] -> (Int, Int) -> [Int]
pairToResult mem (noun, verb) = runProgram . set 1 noun . set 2 verb $ mem

memToWinningPair :: [Int] -> (Int, Int)
memToWinningPair mem =
  head . filter ((== 19690720) . head . (pairToResult mem)) $
  [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]

inputToMem :: String -> [Int]
inputToMem = map (read :: String -> Int) . splitOn ','

main :: IO ()
main = fmap inputToAnswer getContents >>= putStrLn
  where
    inputToAnswer =
      show .
      (\(noun, verb) -> 100 * noun + verb) . memToWinningPair . inputToMem
