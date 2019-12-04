import Control.Monad (replicateM)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Set (fromList, intersection, toList)

type Coord = (Int, Int)

coords :: [String] -> [Coord]
coords = foldl addLine [(0, 0)]
  where
    addLine :: [Coord] -> String -> [Coord]
    addLine prev (d:nstr) =
      let n = read nstr :: Int
          (x, y) = last prev
       in prev ++
          case d of
            'U' -> [(x, y') | y' <- take n $ enumFrom (y + 1)]
            'D' -> [(x, y') | y' <- take n $ enumFromThen (y - 1) (y - 2)]
            'L' -> [(x', y) | x' <- take n $ enumFromThen (x - 1) (x - 2)]
            'R' -> [(x', y) | x' <- take n $ enumFrom (x + 1)]
            _ -> error ("Bad direction: " ++ [d])
    addLine _ _ = error "Bad list entry?"

intersections :: [[Coord]] -> [Coord]
intersections = toList . foldl1 intersection . map fromList

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn splitter = foldl f [[]]
  where
    f splits cur
      | cur == splitter = mappend splits [[]]
      | otherwise = init splits ++ [(last splits ++ [cur])]

minSignalDelay :: [[Coord]] -> Int
minSignalDelay gridCoords =
  minimum $ map signalDelay . intersections $ gridCoords
  where
    signalDelay :: Coord -> Int
    -- The (+1) accounts for dropping the origin from our coord lists
    signalDelay c = sum $ map ((+ 1) . fromJust . elemIndex c) gridCoords

main :: IO ()
main =
  replicateM 2 getLine >>=
  putStrLn .
  show .
  minSignalDelay .
  -- We drop the first coordiante, the origin
  map (drop 1 . coords) . map (splitOn ',')
