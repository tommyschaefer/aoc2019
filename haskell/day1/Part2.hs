module Part2 where

fuel :: Int -> Int
fuel mass
    | m <= 0    = 0
    | otherwise = m + fuel m
    where m = mass `div` 3 - 2

parseInput :: String -> [Int]
parseInput = map read . lines

requiredFuel :: String -> Int
requiredFuel = sum . map fuel . parseInput

main :: IO ()
main = interact $ show . requiredFuel
