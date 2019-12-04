type Mass = Int

type Fuel = Int

massToFuel :: Mass -> Fuel
massToFuel = subtract 2 . floor . (/ 3) . fromIntegral

fuelFuel :: Fuel -> Fuel
fuelFuel f = go f f
  where
    go :: Fuel -> Fuel -> Fuel
    go total 0 = max 0 total
    go total f =
      let ff = max 0 (massToFuel f)
       in go (total + ff) ff

main :: IO ()
main = fmap inputToAnswer getContents >>= putStrLn
  where
    inputToAnswer = show . sum . map (fuelFuel . massToFuel . read) . lines
