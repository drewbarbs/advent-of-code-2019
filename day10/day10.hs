import Data.Functor ((<&>))
import Data.List (concat)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sort (sortOn)

type Point = (Rational, Rational)

sub :: Point -> Point -> Point
sub (a, b) (c, d) = (a - c, b - d)

add :: Point -> Point -> Point
add (a, b) (c, d) = (a + c, b + d)

scalarMult :: Rational -> Point -> Point
scalarMult c (a, b) = (c * a, c * b)

norm :: Floating a => Point -> a
norm (a, b) = sqrt $ fromRational $ (a * a) + (b * b)

asteroids :: [String] -> [Point]
asteroids = concat . map (uncurry parseLine) . zip [0 ..]
  where
    parseLine :: Rational -> String -> [Point]
    parseLine y =
      map ((flip (,) $ y) . fst) . filter ((== '#') . snd) . zip [0 ..]

onLine :: Point -> Point -> Point -> Bool
onLine from to p = t > 0 && t < 1 && p == add from (scalarMult t delta)
  where
    delta = sub to from
    t =
      if fst delta /= 0
        then (fst p - fst from) / fst delta
        else (snd p - snd from) / snd delta

isVisibleFrom :: Set Point -> Point -> Point -> Bool
isVisibleFrom all dst src =
  if dst == src
    then False
    else not $ any id $ map (onLine dst src) rest
  where
    rest :: [Point]
    rest = Set.toList $ Set.delete dst $ Set.delete src all

nVisibleFrom :: Set Point -> Point -> Int
nVisibleFrom all p =
  sum $ map toInt $ map (isVisibleFrom all p) $ Set.toList all
  where
    toInt False = 0
    toInt True = 1

angleFromTo :: Floating a => Point -> Point -> a
angleFromTo p q = fix angle
  where
    angle =
      let opposite = fromRational $ fst q - fst p
          adjacent = negate $ fromRational $ snd q - snd p
          hypotenuse = norm (sub p q)
       in if (snd q - snd p) /= 0
            then atan (opposite / adjacent)
            else asin (opposite / hypotenuse)
    fix a =
      case () of
        _
          | fst q >= fst p && snd q < snd p -> a
          | snd q >= snd p -> pi + a
          | otherwise -> 2 * pi + a

hitOrderFrom :: Set Point -> Point -> Int -> [(Int, Point)]
hitOrderFrom all p startAt =
  let rest = Set.delete p all
      visible = Set.filter (isVisibleFrom rest p) rest
      notVisible = Set.difference rest visible
   in zip [startAt ..] (sortOn (angleFromTo p) $ Set.toList visible) ++
      hitOrderFrom notVisible p (startAt + length visible)

main :: IO ()
main =
  getContents <&> lines <&> asteroids <&> Set.fromList <&>
  (\all ->
     let station = head $ reverse $ sortOn (nVisibleFrom all) (Set.toList all)
      in hitOrderFrom all station 1) <&>
  dropWhile ((/=) 200 . fst) <&>
  head <&>
  snd <&>
  show >>=
  putStrLn
