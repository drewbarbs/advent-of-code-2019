import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

import Text.Regex.PCRE

type Chemical = String

data ChemQty =
  ChemQty Chemical Int
  deriving (Show)

data Reaction =
  Reaction
    { ingredients :: [ChemQty]
    , output :: ChemQty
    }
  deriving (Show)

getName :: ChemQty -> Chemical
getName (ChemQty name _) = name

getQty :: ChemQty -> Int
getQty (ChemQty _ qty) = qty

parseChemQty :: String -> ChemQty
parseChemQty s =
  let (_:qStr:name:[]):[] = s =~ "(\\d+) ([A-Z]+)" :: [[String]]
   in ChemQty name (read qStr :: Int)

parseReaction :: String -> Reaction
parseReaction s =
  let qtys =
        map parseChemQty $
        (getAllTextMatches $ s =~ "(\\d+) ([A-Z]+)" :: [String])
   in Reaction {ingredients = init qtys, output = last qtys}

dominatorTree :: Map Chemical Reaction -> Chemical -> Map Int [Chemical]
dominatorTree reactions chem =
  Map.foldrWithKey (\c b t -> Map.insertWith (++) b (c : []) t) Map.empty $
  go (Map.fromList [("ORE", 0)]) chem
  where
    go :: Map Chemical Int -> Chemical -> Map Chemical Int
    go chemRanks c =
      if Map.member c chemRanks
        then chemRanks
        else let deps =
                   map getName $ ingredients $ fromJust $ Map.lookup c reactions
                 chemRanks' = foldl go chemRanks deps
                 depRanks = map (fromJust . (flip Map.lookup) chemRanks') deps
              in Map.insert c (maximum depRanks + 1) chemRanks'

ingredientsRequired :: Map Chemical Reaction -> ChemQty -> Map Chemical Int
ingredientsRequired reactions (ChemQty chem n) =
  let (Reaction deps (ChemQty _ m)) = fromJust $ Map.lookup chem reactions
      multiplier = ceiling (fromIntegral n / fromIntegral m) :: Int
   in Map.fromList $ map (\cq -> (getName cq, multiplier * getQty cq)) deps

oreRequired :: Map Chemical Reaction -> ChemQty -> Int
oreRequired rMap (ChemQty chem n) = go (Map.fromList [(chem, n)]) maxDepth
  where
    dTree = dominatorTree rMap chem
    (maxDepth, _) = Map.findMax dTree
    go :: Map Chemical Int -> Int -> Int
    go needed 0 = fromJust $ Map.lookup "ORE" needed
    go needed depth =
      let toReduce = fromJust $ Map.lookup depth dTree
          subtree = Map.filterWithKey (const . ((flip elem) toReduce)) needed
          needed' = Map.difference needed subtree
          resolved =
            Map.foldrWithKey
              (\c n' m ->
                 Map.unionWith (+) (ingredientsRequired rMap (ChemQty c n')) m)
              needed'
              subtree
       in go resolved (depth - 1)

bsearch :: Map Chemical Reaction -> Int -> Int -> Int
bsearch rMap rangeStart rangeEnd =
  let midPoint = rangeStart + (rangeEnd - rangeStart) `div` 2
      midValid = (oreRequired rMap (ChemQty "FUEL" midPoint)) < 1000000000000
   in case () of
        _
          | midPoint == rangeStart && midValid -> midPoint
          | midPoint == rangeStart -> error "Couldn't find?"
          | midValid -> bsearch rMap midPoint rangeEnd
          | otherwise -> bsearch rMap rangeStart midPoint

main :: IO ()
main = do
  reactions <- fmap (map parseReaction . lines) getContents
  let rMap = Map.fromList $ [(getName $ output r, r) | r <- reactions]
  let part1 = oreRequired rMap (ChemQty "FUEL" 1)
  putStrLn $ show (part1, bsearch rMap 0 1000000000000)
  return ()
