import Control.Arrow ((&&&))
import Data.Functor ((<&>))
import Text.Regex.PCRE

-- updateVelocities: function of gravity
-- updatePositions: apply velocities
type Position = (Int, Int, Int)

type Velocity = (Int, Int, Int)

data Moon =
  Moon Position Velocity
  deriving (Eq, Show)

energy :: (Int, Int, Int) -> Int
energy (x, y, z) = abs x + abs y + abs z

kEnergy :: Moon -> Int
kEnergy (Moon pos velocity) = energy pos * energy velocity

-- from https://www.reddit.com/r/haskell/comments/22s7r2/clamp/
clamp :: Int -> Int -> Int -> Int
clamp mn mx = min mx . max mn

sub :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
sub (x, y, z) (x', y', z') = (x - x', y - y', z - z')

add :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
add (x, y, z) (x', y', z') = (x + x', y + y', z + z')

-- Compute change in velocity due to effect of gravity from the second moon on the first
acceleration :: Moon -> Moon -> (Int, Int, Int)
acceleration (Moon p _) (Moon p' _) =
  let clamped = clamp (-1) 1
      (dx, dy, dz) = sub p' p
   in (clamped dx, clamped dy, clamped dz)

applyGravity :: [Moon] -> [Moon]
applyGravity (m:ms) = reverse $ go [] m ms
  where
    go :: [Moon] -> Moon -> [Moon] -> [Moon]
    go updated cur@(Moon p v) rest
      | length updated == length rest + 1 = updated
      | otherwise =
        let v' = foldr add v $ map (acceleration cur) rest
            cur' = Moon p v'
         in go (cur' : updated) (head rest) (tail rest ++ [cur])

applyVelocity :: [Moon] -> [Moon]
applyVelocity = map update
  where
    update :: Moon -> Moon
    update (Moon p v) = Moon (add p v) v

timeStep :: [Moon] -> [Moon]
timeStep = applyVelocity . applyGravity

parseMoon :: String -> Moon
parseMoon s =
  let (x:y:z:[]) = getAllTextMatches $ s =~ "[-\\d]+" :: ([String])
      pos = (read x :: Int, read y :: Int, read z :: Int)
   in Moon pos (0, 0, 0)

part1 :: [Moon] -> Int
part1 = last . take 1000 . drop 1 . iterate timeStep <&> sum . map kEnergy

getVelocity :: Moon -> Velocity
getVelocity (Moon _ v) = v

getPosition :: Moon -> Position
getPosition (Moon p _) = p

-- Key observation for part 2 is that x, y, z are all *independent*,
-- so there should be a period for the series of x values, another for
-- y, and another for z. Then period of the whole thing is the LCM of
-- the periods of all three
part2 :: [Moon] -> Int
part2 moons =
  let fst' (a, _, _) = a
      snd' (_, b, _) = b
      thd' (_, _, c) = c
      stateSeq = drop 1 $ iterate timeStep moons
      getVelocities tupGetter = map (tupGetter . getVelocity)
      getPositions tupGetter = map (tupGetter . getPosition)
      initXState = zip (getPositions fst' moons) (getVelocities fst' moons)
      initYState = zip (getPositions snd' moons) (getVelocities snd' moons)
      initZState = zip (getPositions thd' moons) (getVelocities thd' moons)
      xperiod =
        (+ 1) . length $
        takeWhile
          ((/= initXState) .
           (\ms -> zip (getPositions fst' ms) (getVelocities fst' ms))) $
        stateSeq
      yperiod =
        (+ 1) . length $
        takeWhile
          ((/= initYState) .
           (\ms -> zip (getPositions snd' ms) (getVelocities snd' ms))) $
        stateSeq
      zperiod =
        (+ 1) . length $
        takeWhile
          ((/= initZState) .
           (\ms -> zip (getPositions thd' ms) (getVelocities thd' ms))) $
        stateSeq
   in foldr lcm 1 [xperiod, yperiod, zperiod]

main :: IO ()
main =
  getContents <&> map parseMoon . lines >>= putStrLn . show . (part1 &&& part2)
