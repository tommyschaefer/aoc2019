module Part1 where

import           Data.List.Split (splitOn)

type Program = [Int]

data Instruction
    = Add Int Int Int
    | Multiply Int Int Int
    | Stop
    deriving (Show)

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

answer :: String -> Int
answer = head . run . restore 12 2 . parseProg

main :: IO ()
main = interact $ show . answer
