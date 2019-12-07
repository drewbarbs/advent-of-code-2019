import Common (Tree(..), parseTree)

type Checksums = (Int, Int) -- (# direct orbits, # indirect)

checksums :: Tree -> Checksums
checksums = go 0
  where
    sumTups :: (Int, Int) -> (Int, Int) -> (Int, Int)
    sumTups (a, b) (c, d) = (a + c, b + d)
    sumAll = foldr sumTups (0, 0)
    go :: Int -> Tree -> Checksums
    go 0 (Node _ children) = sumAll $ map (go 1) children
    go depth (Node _ children) =
      let (direct, indirect) = sumAll $ map (go (depth + 1)) children
       in (direct + 1, indirect + (depth - 1))

main :: IO ()
main =
  getContents >>=
  putStrLn . show . (\(a, b) -> a + b) . checksums . parseTree . lines
