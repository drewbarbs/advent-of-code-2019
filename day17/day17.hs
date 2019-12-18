import Data.Char (chr, ord)
import Data.List (intercalate)

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

type GridPosition = (Int, Int)

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

interpretCameraOutput :: Int -> Char
interpretCameraOutput = chr

part1Run :: RunState -> IO [GridPosition]
part1Run runState = go (0, 0) [] runState
  where
    go :: GridPosition -> [GridPosition] -> RunState -> IO [GridPosition]
    go curPos@(x, y) curList s =
      case runUntilIO s of
        Done -> return curList
        Output i state ->
          case interpretCameraOutput i of
            '\n' -> go (0, y + 1) curList state
            '.' -> go (x + 1, y) curList state
            '#' -> go (x + 1, y) (curPos : curList) state
            '^' -> go (x + 1, y) (curPos : curList) state
            '<' -> go (x + 1, y) (curPos : curList) state
            '>' -> go (x + 1, y) (curPos : curList) state
            'v' -> go (x + 1, y) (curPos : curList) state
        Input handler -> error "Unexpected input prompt"

part1 :: Mem -> IO ()
part1 prog = do
  scaffoldPositions <- part1Run (prog ++ repeat 0, 0, 0)
  let alignParams =
        [ x * y
        | (x, y) <- scaffoldPositions
        , (x + 1, y) `elem` scaffoldPositions &&
            (x - 1, y) `elem` scaffoldPositions &&
            (x, y + 1) `elem` scaffoldPositions &&
            (x, y - 1) `elem` scaffoldPositions
        ]
  putStrLn $ ("Sum of alignment parameters: " ++) . show $ sum alignParams

part2 :: Mem -> IO ()
part2 mem = go inputSeq startState
  where
    moveRoutine = ["A", "B", "A", "B", "A", "C", "B", "C", "A", "C"]
    pA = ["L", "10", "L", "12", "R", "6"]
    pB = ["R", "10", "L", "4", "L", "4", "L", "12"]
    pC = ["L", "10", "R", "10", "R", "6", "L", "4"]
    promptAnswer = ["n", "\n"]
    inputSeq :: [Int]
    inputSeq =
      map ord $
      intercalate "\n" $
      map (intercalate ",") [moveRoutine, pA, pB, pC, promptAnswer]
    startState = ((set 0 2 mem) ++ repeat 0, 0, 0)
    go :: [Int] -> RunState -> IO ()
    go inputs runState =
      case runUntilIO runState of
        Done -> putStrLn "Done"
        Output chrCode nextState -> do
          if chrCode < 128
            then putChar (chr chrCode)
            else putStrLn ("Dust collected: " ++ show chrCode)
          go inputs nextState
        Input handler -> go (tail inputs) (handler $ head inputs)

main :: IO ()
main = do
  prog <- readFile "input"
  let program = map (read :: String -> Int) $ splitOn ',' prog
  part1 program
  part2 program
