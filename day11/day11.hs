import Control.Arrow ((&&&))
import Data.List (nub)
import Data.Maybe (fromJust, fromMaybe)
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

type GridPosition = (Int, Int)

data Color
  = White
  | Black
  deriving (Eq, Show)

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

outputToColor :: Int -> Color
outputToColor 0 = Black
outputToColor 1 = White
outputToColor n = error ("Undefined color: " ++ show n)

colorToInput :: Color -> Int
colorToInput Black = 0
colorToInput White = 1

getCurrentColor :: GridPosition -> [(GridPosition, Color)] -> Color
getCurrentColor = (fromMaybe Black .) . lookup

rotateHeading :: (Int, Int) -> Int -> (Int, Int)
rotateHeading (dx, dy) out =
  let theta =
        if out == 0
          then pi / (2 :: Float)
          else (-pi) / (2 :: Float)
   in ( dx * round (cos theta) - dy * round (sin theta)
      , dx * round (sin theta) + dy * round (cos theta))

add :: GridPosition -> (Int, Int) -> GridPosition
add (a, b) (c, d) = (a + c, b + d)

getPanelPaintActions ::
     RunState
  -> (GridPosition -> [(GridPosition, Color)] -> Color)
  -> [(GridPosition, Color)]
getPanelPaintActions runState colorGetter = go runState [] (0, 0) (0, 1)
  where
    go ::
         RunState
      -> [(GridPosition, Color)]
      -> GridPosition
      -> (Int, Int)
      -> [(GridPosition, Color)]
    go state actions curPos heading =
      case runUntilIO state of
        Done -> actions
        Output cNum state' ->
          let Output dirOutput state'' = runUntilIO state'
              actions' = (curPos, outputToColor cNum) : actions
              heading' = rotateHeading heading dirOutput
           in go state'' actions' (add curPos heading') heading'
        Input handler ->
          let input = colorToInput $ colorGetter curPos actions
           in go (handler input) actions curPos heading

part1 :: Mem -> Int
part1 mem =
  let actions = getPanelPaintActions (mem, 0, 0) getCurrentColor
      nPainted = length $ nub $ map fst actions
   in nPainted

part2ColorGetter :: GridPosition -> [(GridPosition, Color)] -> Color
part2ColorGetter _ [] = White
part2ColorGetter p actions = getCurrentColor p actions

part2 :: Mem -> String
part2 mem =
  let actions = getPanelPaintActions (mem, 0, 0) part2ColorGetter
      uniquePositions = nub $ map fst actions
      lookupInActions = (\p -> (p, (fromJust . (flip lookup $ actions)) p))
      coloredPoints = map lookupInActions uniquePositions
      pointToLine = (\(x, y) -> show x ++ " " ++ show y)
   in unlines $
      map pointToLine $ map fst $ filter ((== White) . snd) coloredPoints

handleResults :: (Int, String) -> IO ()
handleResults (count, plotData) = do
  putStrLn ("Part 1: " ++ show count)
  writeFile "points.txt" plotData
  putStrLn ("Part 2 written to points.txt")

main :: IO ()
main =
  let inputToProg = (++ [0 ..]) . map (read :: String -> Int) . splitOn ','
   in fmap inputToProg (readFile "input") >>= handleResults . (part1 &&& part2)
