module Part1 where

data Instruction = Add Int Int Int | Multiply Int Int Int| Stop deriving Show

wordsBy :: Char -> String -> [String]
wordsBy sep str = case dropWhile matchesSep str of
    "" -> []
    s -> w : wordsBy sep s' where (w, s') = break matchesSep s
    where matchesSep = (== sep)

update :: (Int -> Int -> Int) -> Int -> Int -> Int -> [Int] -> [Int]
update f a b c xs = head ++ [v] ++ tail
    where v = f (xs !! a) (xs !! b)
          (head,_:tail) = splitAt c xs

opcode :: Int -> [Int] -> (Instruction, [Int])
opcode idx list = case list' of
    (1:a:b:c:_) -> (Add a b c, list)
    (2:a:b:c:_) -> (Multiply a b c, list)
    (99:_) -> (Stop, list)
    where (_, list') = splitAt idx list

calculate :: Int -> [Int] -> [Int]
calculate idx xs = case opcode idx xs of
    (Add a b c, list) -> calculate (idx + 4) $ update (+) a b c list
    (Multiply a b c, list) -> calculate (idx + 4) $ update (*) a b c list
    (Stop, list) -> list

restore :: [Int] -> [Int]
restore (a:_:_:xs) = (a:12:2:xs)

parseInput :: String -> [Int]
parseInput = map read . wordsBy ','

answer :: String -> Int
answer = head . calculate 0 . restore . parseInput

main :: IO ()
main = interact $ show . answer
