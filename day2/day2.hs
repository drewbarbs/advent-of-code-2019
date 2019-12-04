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

main :: IO ()
main = fmap inputToAnswer getContents >>= putStrLn
  where
    inputToAnswer =
      show .
      runProgram .
      set 1 12 . set 2 2 . map (read :: String -> Int) . splitOn ','
