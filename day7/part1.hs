import Data.List (permutations)

import Common

runAmplifier :: Mem -> Int -> Int -> Int
runAmplifier prog input phase =
  let (Input phaseHandler) = runUntilIO (prog, 0)
      (Input inputHandler) = runUntilIO (phaseHandler phase)
      (Output output _) = runUntilIO (inputHandler input)
   in output

runSequence :: Mem -> [Int] -> Int
runSequence mem = foldl (runAmplifier mem) 0

main :: IO ()
main = do
  prog <- getContents
  let program = map (read :: String -> Int) $ splitOn ',' prog
  let optimal = maximum $ map (runSequence program) $ permutations [0 .. 4]
  putStrLn $ show optimal
