module Part1 where

import           Data.List       (find)
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromMaybe)

data Instruction
    = Add Int Int Int
    | Multiply Int Int Int
    | Stop
    deriving (Show)

combinations :: [a] -> [b] -> [(a, b)]
combinations as bs = [(a, b) | a <- as, b <- bs]

updateWith :: Int -> ([a] -> a) -> [a] -> [a]
updateWith i f vs = head ++ f vs:tail where (head, _:tail) = splitAt i vs

addressValues :: Int -> Int -> [Int] -> (Int, Int)
addressValues pa pb memory = (memory !! pa, memory !! pb)

applyToAddresses :: (Int -> Int -> Int) -> Int -> Int -> [Int] -> Int
applyToAddresses f pa pb = uncurry f . addressValues pa pb

applyInstruction :: Instruction -> [Int] -> [Int]
applyInstruction (Add pa pb pc)      = updateWith pc $ applyToAddresses (+) pa pb
applyInstruction (Multiply pa pb pc) = updateWith pc $ applyToAddresses (*) pa pb

instructionAt :: Int -> [Int] -> Instruction
instructionAt address memory = case list' of
    (1:a:b:c:_) -> Add a b c
    (2:a:b:c:_) -> Multiply a b c
    (99:_)      -> Stop
  where
    (_, list') = splitAt address memory

calculate :: Int -> [Int] -> [Int]
calculate address memory = case instructionAt address memory of
    Stop -> memory
    i    -> calculate (address + 4) $ applyInstruction i memory

restore :: Int -> Int -> [Int] -> [Int]
restore n v (a:_:_:xs) = a:n:v:xs

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

compute :: Int -> Int -> String -> Int
compute n v = head . calculate 0 . restore n v . parseInput

isTarget :: String -> (Int, Int) -> Bool
isTarget s (n, v) = 19690720 == compute n v s

validSolution :: String -> Maybe (Int, Int)
validSolution s = find (isTarget s) $ combinations [0..99] [0..99]

answer :: String -> Maybe Int
answer s = format <$> validSolution s
    where format (n, v) = 100 * n + v

main :: IO ()
main = interact $ show . answer
