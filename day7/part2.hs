import Data.List (permutations)

import Common

-- Amplifier on first run will prompt for phase assignment, then input
runAmplifierInitialMode :: Mem -> ([RunState], Int) -> Int -> ([RunState], Int)
runAmplifierInitialMode prog (states, input) phase =
  let (Input phaseHandler) = runUntilIO (prog, 0)
      (Input inputHandler) = runUntilIO (phaseHandler phase)
      (Output output state) = runUntilIO (inputHandler input)
   in (states ++ [state], output)

runInitial :: Mem -> [Int] -> ([RunState], Int)
runInitial mem = foldl (runAmplifierInitialMode mem) ([], 0)

-- Amplifier after first run will only expect 1 input (output of previous stage), produce 1 output, *or* exit
runAmplifierFeedbackMode ::
     Maybe ([RunState], Int) -> RunState -> Maybe ([RunState], Int)
runAmplifierFeedbackMode Nothing _ = Nothing
runAmplifierFeedbackMode (Just (states, input)) state =
  case runUntilIO state of
    (Input inputHandler) ->
      let (Output output outState) = runUntilIO $ inputHandler input
       in Just (states ++ [outState], output)
    Done -> Nothing
    Output _ _ -> error "starting amplifier in output mode?"

runFeedbackLoop :: [RunState] -> Int -> Int
runFeedbackLoop states lastEOutput =
  case foldl runAmplifierFeedbackMode (Just ([], lastEOutput)) states of
    Nothing -> lastEOutput
    Just (newStates, newEOutput) -> runFeedbackLoop newStates newEOutput

runPhaseAssignment :: Mem -> [Int] -> Int
runPhaseAssignment prog phaseAssignment =
  let (ampStates, eOutput) = runInitial prog phaseAssignment
   in runFeedbackLoop ampStates eOutput

main :: IO ()
main = do
  prog <- getContents
  let program = map (read :: String -> Int) $ splitOn ',' prog
  let optimal =
        maximum $ map (runPhaseAssignment program) $ permutations [5 .. 9]
  putStrLn $ show optimal
