digits :: (Eq n, Integral n) => n -> [n]
digits = go [] . (flip quotRem) 10
  where
    go :: (Integral n, Eq n) => [n] -> (n, n) -> [n]
    go digs (0, r) = r : digs
    go digs (q, r) = go (r : digs) (quotRem q 10)

valid :: (Eq n, Integral n) => n -> Bool
valid n = go False (head $ digits n) (tail $ digits n)
  where
    go :: (Eq n, Ord n) => Bool -> n -> [n] -> Bool
    go sawRepeat _ [] = sawRepeat
    go sawRepeat lastDigit (d:ds) =
      if d >= lastDigit
        then go (sawRepeat || lastDigit == d) d ds
        else False

answer :: Int -> Int -> Int
answer start end = length . filter valid $ enumFromTo start end

main :: IO ()
main = do
  line <- getLine
  let a = (read (takeWhile (/= '-') line) :: Int)
  let b = (read (drop 1 (dropWhile (/= '-') line)) :: Int)
  putStrLn $ show $ answer a b
