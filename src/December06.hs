module December06 where

import Data.List(elemIndex)
import Data.Maybe(fromJust)
import Debug.Trace

newtype MemoryBank = MemoryBank { blocks:: Int} deriving (Show, Eq, Ord)

increment :: MemoryBank -> MemoryBank
increment (MemoryBank x) = MemoryBank (x + 1)

cyclesBetweenRepetitions :: [MemoryBank] -> Int
cyclesBetweenRepetitions memoryBank =
 let configurations = [ memoryBank ]
 in snd $ redistribution configurations 1 memoryBank

redistributionCycleTillAlreadySeenConfiguration :: [MemoryBank] -> Int
redistributionCycleTillAlreadySeenConfiguration memoryBank =
 let configurations = [ memoryBank ]
 in fst $ redistribution configurations 1 memoryBank


redistribution :: [[MemoryBank]] -> Int -> [MemoryBank] -> (Int,Int)
redistribution configurations cycleCount memoryBanks
  | newBank `elem` configurations = (cycleCount, cycleCount - fromJust(elemIndex newBank configurations))
  | otherwise                     = redistribution newConfigurations (cycleCount + 1) newBank
  where newBank = redistribute memoryBanks $ maximumWithIndex memoryBanks
        newConfigurations = configurations ++ [newBank]



maximumWithIndex :: [ MemoryBank ] -> (Int, Int)
maximumWithIndex xs = let maxMemoryBank = maximum xs
                          maxBlockBankIndex = elemIndex maxMemoryBank xs
                      in (blocks maxMemoryBank, fromJust maxBlockBankIndex)

redistribute :: [MemoryBank] -> (Int, Int) -> [MemoryBank]
redistribute banks (maxBlocks, maxBlocksIndex) =
 let (before, _:after) = splitAt maxBlocksIndex banks
     withClearedBank = before ++ [MemoryBank 0] ++ after
 in evenDistribute maxBlocks (maxBlocksIndex + 1) withClearedBank

evenDistribute :: Int -> Int -> [MemoryBank] -> [MemoryBank]
evenDistribute 0 _ banks = banks
evenDistribute blocks index banks =
 let currentIndex = index `mod` length banks
     remainingBlocks = blocks - 1
     (before, bank:after) = splitAt currentIndex banks
     adjustedBank = before ++ [increment bank] ++ after
 in evenDistribute remainingBlocks (currentIndex+1) adjustedBank

