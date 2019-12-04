type Mass = Int

type Fuel = Int

fuel :: Mass -> Fuel
fuel = subtract 2 . floor . (/ 3) . fromIntegral

main :: IO ()
main = fmap inputToAnswer getContents >>= putStrLn
  where
    inputToAnswer = show . sum . map (fuel . read) . lines
