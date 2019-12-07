module Common
  ( Tree(..)
  , parseTree
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

data Tree =
  Node String [Tree]
  deriving (Show)

parseLine :: String -> (String, String)
parseLine l =
  case (takeWhile (/= ')') l, tail $ dropWhile (/= ')') l) of
    ([], _) -> error "Empty parent name!"
    (_, []) -> error "Empty child name!"
    (p, c) -> (p, c)

parseTree :: [String] -> Tree
parseTree edges = toTree childMap "COM"
  where
    go :: [String] -> Map String [String] -> Map String [String]
    go [] nodeMap = nodeMap
    go (l:rest) nodeMap =
      let (parentKey, childKey) = parseLine l
          children = Map.findWithDefault [] parentKey nodeMap
          newMap = Map.insert parentKey (childKey : children) nodeMap
       in go rest $ newMap
    childMap :: Map String [String]
    childMap = go edges Map.empty
    toTree :: Map String [String] -> String -> Tree
    toTree m rootName =
      Node rootName $
      map (\n -> toTree m n) $ fromMaybe [] $ Map.lookup rootName m
