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

restore :: (Int, Int) -> [Int] -> [Int]
restore (noun,verb) (a:_:_:xs) = (a:noun:verb:xs)

parseInput :: String -> [Int]
parseInput = map read . wordsBy ','

answerAttempt :: (Int, Int) -> String -> Int
answerAttempt nv = head . calculate 0 . restore nv . parseInput

combine :: [Int] -> [Int] -> [(Int, Int)]
combine xs ys = [(x,y) | x<-xs, y<-ys]

findAnswer :: Int -> [(Int, Int)] -> String -> (Int, Int)
findAnswer v (nv:nvs) s
    | v /= (answerAttempt nv s) = findAnswer v nvs s
    | otherwise               = nv

answer :: String -> Int
answer s =  100 * n + v
    where (n, v) = findAnswer 19690720 (combine [0..99] [0..99]) s

main :: IO ()
main = interact $ show . answer
