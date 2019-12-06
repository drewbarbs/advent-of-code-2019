import System.IO (hFlush, stdout)

type PC = Int

type Mem = [Int]

type RunState = (Mem, PC)

data RunResult
  = Output Int RunState
  | Input (Int -> RunState)
  | Done

set :: Int -> Int -> [Int] -> [Int]
set idx v xs = (take idx xs) ++ [v] ++ (drop (idx + 1) xs)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn splitter = foldl f [[]]
  where
    f splits cur
      | cur == splitter = mappend splits [[]]
      | otherwise = init splits ++ [(last splits ++ [cur])]

digits :: (Eq n, Integral n) => n -> [n]
digits = go [] . (flip quotRem) 10
  where
    go :: (Integral n, Eq n) => [n] -> (n, n) -> [n]
    go digs (0, r) = r : digs
    go digs (q, r) = go (r : digs) (quotRem q 10)

parseOpCode :: Int -> (Int, [Int])
parseOpCode opc = (opNum, drop 2 revDigs)
  where
    revDigs = (reverse . digits $ opc) ++ (repeat 0)
    opNum = head revDigs + 10 * (revDigs !! 1)

paramValue :: Mem -> Int -> Int -> Int
paramValue mem paramMode instrParam =
  case paramMode of
    0 -> mem !! instrParam
    1 -> instrParam
    _ -> error "Unrecognized paramMode"

runUntilIO :: RunState -> RunResult
runUntilIO (mem, pc) =
  let (opCode, paramModes) = parseOpCode (mem !! pc)
      iParams = drop (pc + 1) mem
      pVals =
        [paramValue mem mode iParam | (mode, iParam) <- zip paramModes iParams]
   in case opCode of
        _
          | opCode == 99 -> Done
          | opCode == 3 -> Input (\i -> (set (head iParams) i mem, pc + 2))
          | opCode == 4 -> Output (head pVals) (mem, pc + 2)
          | opCode == 5 || opCode == 6 ->
            let cmp =
                  if opCode == 5
                    then (/=)
                    else (==)
                nextPC =
                  if (cmp (head pVals) 0)
                    then (pVals !! 1)
                    else pc + 3
             in runUntilIO (mem, nextPC)
          | opCode == 7 || opCode == 8 ->
            let cmp =
                  if opCode == 7
                    then (<)
                    else (==)
                v =
                  if (cmp (pVals !! 0) (pVals !! 1))
                    then 1
                    else 0
             in runUntilIO (set (iParams !! 2) v mem, pc + 4)
          | opCode == 1 || opCode == 2 ->
            let op =
                  if opCode == 1
                    then (+)
                    else (*)
                v = op (pVals !! 0) (pVals !! 1)
             in runUntilIO (set (iParams !! 2) v mem, pc + 4)

runProgram :: RunState -> IO ()
runProgram runState =
  case runUntilIO runState of
    Done -> putStrLn "Done"
    Output i state ->
      mconcat
        [ (putStr . ("Output: " ++) . show $ i)
        , (putStrLn "")
        , (runProgram state)
        ]
    Input handler -> do
      putStr "Input: "
      hFlush stdout
      l <- getLine
      let n = (read :: String -> Int) l
      runProgram . handler $ n

main :: IO ()
main = do
  strInput <- readFile "input"
  let program = map (read :: String -> Int) $ splitOn ',' strInput
  runProgram (program, 0)
