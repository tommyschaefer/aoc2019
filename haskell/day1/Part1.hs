module Part1 where

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

parseInput :: String -> [Int]
parseInput = map read . lines

requiredFuel :: String -> Int
requiredFuel = sum . map fuel . parseInput

main :: IO ()
main = interact $ show . requiredFuel
