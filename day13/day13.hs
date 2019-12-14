import Control.Arrow ((&&&))
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Text.Printf
import UI.NCurses

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

data Tile
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball

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

getTile :: Int -> Tile
getTile 0 = Empty
getTile 1 = Wall
getTile 2 = Block
getTile 3 = Paddle
getTile 4 = Ball
getTile _ = error "Unrecognized tile id"

runWithoutInput :: Map GridPosition Tile -> RunState -> Map GridPosition Tile
runWithoutInput mp runState =
  case runUntilIO runState of
    Done -> mp
    Output x waitYState ->
      let (Output y waitTIDState) = runUntilIO waitYState
          (Output tileId resumeState) = runUntilIO waitTIDState
          updated = Map.insert (x, y) (getTile tileId) mp
       in runWithoutInput updated resumeState
    _ -> error "Prompted for input!"

isBlk :: Tile -> Int
isBlk Block = 1
isBlk _ = 0

part1 :: Mem -> IO ()
part1 mem = do
  let objs = runWithoutInput Map.empty (mem, 0, 0)
  let maxX = Map.foldrWithKey (\(x, _) _ x' -> max x x') (-1) objs
  let maxY = Map.foldrWithKey (\(_, y) _ y' -> max y y') (-1) objs
  let blkCount = foldr ((+) . isBlk) 0 objs
  putStrLn $ printf "Max x: %d, Max y: %d, block count: %d" maxX maxY blkCount

tileChar :: Tile -> Char
tileChar Empty = ' '
tileChar Wall = '█'
tileChar Block = '▅'
tileChar Paddle = '▃'
tileChar Ball = '●'

-- Reverse engineering indicates memory cells 388, 389 store x, y of
-- the ball Map data starts at index 639, consists of 23 rows of length
-- 37 (two walls, 35 playable spaces). So field is
-- mems[1][639:639+851]
-- offset 614 corresponded to score 6
-- hypothesize that offset at 1668 has array of scores for each block?
-- 386 has score?
--
-- Observed that by setting entire row to paddle tiles, I didnt have to play
-- (see input.hacked)
runGame :: Window -> RunState -> Curses ()
runGame w runState@(mem, pc, relBase) =
  case runUntilIO runState of
    Done -> do
      updateWindow w $ do
        moveCursor 0 40
        drawString "Done!"
      render
    Output x waitYState -> do
      let (Output y waitTIDState) = runUntilIO waitYState
      let (Output tileId resumeState) = runUntilIO waitTIDState
      let upd =
            case (x, y) of
              (-1, 0) -> do
                moveCursor 0 0
                drawString $ show tileId
              _ -> do
                moveCursor (toInteger (y + 1)) (toInteger x)
                drawGlyph
                  Glyph
                    { glyphCharacter = tileChar (getTile tileId)
                    , glyphAttributes = []
                    }
      updateWindow w upd
      render
      runGame w resumeState
    Input handler -> do
      ev <-
        waitFor
          w
          (Just 20)
          (\ev ->
             ev == EventSpecialKey KeyLeftArrow ||
             ev == EventSpecialKey KeyRightArrow || ev == EventCharacter ' ')
      case ev of
        Nothing -> runGame w (handler 0)
        Just (EventSpecialKey KeyLeftArrow) -> runGame w (handler (-1))
        Just (EventSpecialKey KeyRightArrow) -> runGame w (handler 1)
        Just (EventCharacter ' ') -> do
          liftIO $
            hPutStrLn stderr $ show pc ++ " " ++ show relBase ++ " " ++ show mem
          runGame w runState

part2 :: Mem -> IO ()
part2 mem =
  runCurses $
  setCursorMode CursorInvisible >>=
  const
    (do setEcho False
        w <- defaultWindow
        runGame w (set 0 2 mem, 0, 0)
        _ <-
          waitFor
            w
            Nothing
            (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')
        return ())

waitFor :: Window -> Maybe Integer -> (Event -> Bool) -> Curses (Maybe Event)
waitFor w timeOut p = loop
  where
    loop = do
      ev <- getEvent w timeOut
      case ev of
        Nothing -> return Nothing
        Just ev' ->
          if p ev'
            then return (Just ev')
            else loop

main :: IO ()
main = do
  let inputToProg =
        (++ [0, 0, 0, 0, 0, 0]) . map (read :: String -> Int) . splitOn ','
  prog <- fmap inputToProg (readFile "input")
  putStr "Which part?: "
  hFlush stdout
  input <- getLine
  case (read input :: Int) of
    1 -> part1 prog
    2 -> part2 prog
    _ -> putStrLn "Bad input"
