import Control.Monad (replicateM)
import Data.List (sortBy)
import Data.Set (fromList, intersection, toList)

type Coord = (Int, Int)

manhattanDistFromOrigin :: Coord -> Int
manhattanDistFromOrigin (x, y) = abs x + abs y

closestPoint :: [Coord] -> Coord
closestPoint = head . sortBy md
  where
    md :: Coord -> Coord -> Ordering
    md c1 c2 = compare (manhattanDistFromOrigin c1) (manhattanDistFromOrigin c2)

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

intersections :: [Coord] -> [Coord] -> [Coord]
intersections xs ys = toList $ intersection (fromList xs) (fromList ys)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn splitter = foldl f [[]]
  where
    f splits cur
      | cur == splitter = mappend splits [[]]
      | otherwise = init splits ++ [(last splits ++ [cur])]

main :: IO ()
main =
  replicateM 2 getLine >>=
  putStrLn .
  show .
  manhattanDistFromOrigin .
  closestPoint .
  filter (/= (0, 0)) . foldl1 intersections . map coords . map (splitOn ',')
