import Data.Maybe (fromJust, isJust, listToMaybe)

import Common (Tree(..), parseTree)

nodePath :: Tree -> String -> [String]
nodePath t dst = reverse . fromJust $ go [] dst t
  where
    go :: [String] -> String -> Tree -> Maybe [String]
    go p k (Node n children)
      | n == k = Just p
      | otherwise =
        listToMaybe $ map fromJust $ filter isJust $ map (go (n : p) k) children

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix p1 p2 = map fst $ takeWhile (\(a, b) -> a == b) $ zip p1 p2

minTransfers :: Tree -> Int
minTransfers t =
  let pathToYOU = nodePath t "YOU"
      pathToSAN = nodePath t "SAN"
      toDrop = length $ commonPrefix pathToSAN pathToYOU
   in (length $ drop toDrop pathToYOU) + (length $ drop toDrop pathToSAN)

main :: IO ()
main = getContents >>= putStrLn . show . minTransfers . parseTree . lines
