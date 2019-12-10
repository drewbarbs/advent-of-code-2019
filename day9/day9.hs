import System.IO (hFlush, stdout)

type PC = Int

type Mem = [Int]

type RelativeBase = Int

type RunState = (Mem, RelativeBase, PC)

data RunResult
  = Output Int RunState
  | Input (Int -> RunState)
  | Done

data ParamType
  = In
  | Out

type ParamMode = Int

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

parseOpCode :: Int -> (Int, [ParamType], [ParamMode])
parseOpCode opc = (opNum, pTypes, drop 2 revDigs)
  where
    revDigs = (reverse . digits $ opc) ++ (repeat 0)
    opNum = head revDigs + 10 * (revDigs !! 1)
    pTypes =
      case () of
        _
          | opNum == 3 -> [Out]
          | elem opNum [1, 2, 7, 8] -> [In, In, Out]
          | otherwise -> repeat In

paramValue :: Mem -> RelativeBase -> ParamType -> ParamMode -> Int -> Int
paramValue mem relBase paramType paramMode instrParam =
  case (paramType, paramMode) of
    (In, 0) -> mem !! instrParam -- position mode: instruction paramter is an address
    (In, 1) -> instrParam -- immediate mode: instr. param is a literal value
    (In, 2) -> mem !! (relBase + instrParam)
    (Out, 0) -> instrParam
    (Out, 2) -> relBase + instrParam
    _ -> error "Unrecognized paramMode"

runUntilIO :: RunState -> RunResult
runUntilIO (mem, relBase, pc) =
  let (opNum, paramTypes, paramModes) = parseOpCode (mem !! pc)
      iParams = drop (pc + 1) mem
      pVals =
        [ paramValue mem relBase pType pMode iParam
        | (pType, pMode, iParam) <- zip3 paramTypes paramModes iParams
        ]
   in case () of
        _
          | opNum == 99 -> Done
          | opNum == 9 -> runUntilIO (mem, relBase + (head pVals), pc + 2)
          | opNum == 3 ->
            Input (\i -> (set (head pVals) i mem, relBase, pc + 2))
          | opNum == 4 -> Output (head pVals) (mem, relBase, pc + 2)
          | opNum == 5 || opNum == 6 ->
            let cmp =
                  if opNum == 5
                    then (/=)
                    else (==)
                nextPC =
                  if (cmp (head pVals) 0)
                    then (pVals !! 1)
                    else pc + 3
             in runUntilIO (mem, relBase, nextPC)
          | opNum == 7 || opNum == 8 ->
            let cmp =
                  if opNum == 7
                    then (<)
                    else (==)
                v =
                  if (cmp (pVals !! 0) (pVals !! 1))
                    then 1
                    else 0
             in runUntilIO (set (pVals !! 2) v mem, relBase, pc + 4)
          | opNum == 1 || opNum == 2 ->
            let op =
                  if opNum == 1
                    then (+)
                    else (*)
                v = op (pVals !! 0) (pVals !! 1)
             in runUntilIO (set (pVals !! 2) v mem, relBase, pc + 4)

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
  prog <- readFile "input"
  let program = map (read :: String -> Int) $ splitOn ',' prog
  runProgram (program ++ repeat 0, 0, 0)
