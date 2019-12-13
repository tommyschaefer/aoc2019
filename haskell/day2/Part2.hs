module Part1 where

import           Data.List       (find)
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromMaybe)

type Program = [Int]

data Instruction
    = Add Int Int Int
    | Multiply Int Int Int
    | Stop
    deriving (Show)

combinations :: [a] -> [b] -> [(a, b)]
combinations as bs = [(a, b) | a <- as, b <- bs]

updateWith :: Int -> ([a] -> a) -> [a] -> [a]
updateWith i f vs = head ++ f vs:tail where (head, _:tail) = splitAt i vs

addressValues :: Int -> Int -> Program -> (Int, Int)
addressValues pa pb prog = (prog !! pa, prog !! pb)

applyToAddresses :: (Int -> Int -> Int) -> Int -> Int -> Program -> Int
applyToAddresses f pa pb = uncurry f . addressValues pa pb

applyInstruction :: Instruction -> Program -> Program
applyInstruction (Add pa pb pc)      = updateWith pc $ applyToAddresses (+) pa pb
applyInstruction (Multiply pa pb pc) = updateWith pc $ applyToAddresses (*) pa pb

instructionAt :: Int -> Program -> Instruction
instructionAt addr prog = case list' of
    (1:a:b:c:_) -> Add a b c
    (2:a:b:c:_) -> Multiply a b c
    (99:_)      -> Stop
  where
    (_, list') = splitAt addr prog

exec :: Int -> Program -> Program
exec addr prog = case instructionAt addr prog of
    Stop -> prog
    i    -> exec (addr + 4) $ applyInstruction i prog

run :: Program -> Program
run = exec 0

restore :: Int -> Int -> Program -> Program
restore n v (a:_:_:xs) = a:n:v:xs

parseProg :: String -> Program
parseProg = map read . splitOn ","

compute :: (Int, Int) -> String -> Int
compute (n, v) = head . run . restore n v . parseProg

computesValue :: Int -> String -> (Int, Int) -> Bool
computesValue v s nv = v == compute nv s

validSolution :: String -> Maybe (Int, Int)
validSolution s = find (computesValue 19690720 s) $ combinations [0..99] [0..99]

answer :: String -> Maybe Int
answer s = format <$> validSolution s
    where format (n, v) = 100 * n + v

main :: IO ()
main = interact $ show . answer
