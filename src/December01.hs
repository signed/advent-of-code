module December01 where

sumOfDigitsMatchingSuccessor :: [Int] -> Int
sumOfDigitsMatchingSuccessor [] = 0
sumOfDigitsMatchingSuccessor [x] = x
sumOfDigitsMatchingSuccessor list@(x : xs) = sum $  map fst (filter equalSuccessor $  zip blub (tail blub))
 where blub = list ++ [x]
       equalSuccessor (a,b) = a == b