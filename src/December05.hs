module December05 where

import Data.Maybe
import qualified Data.IntMap as IntMap

stepsToExit :: [Int] -> Int
stepsToExit list = jump 0 0 initialJumpMap  where
  initialJumpMap = IntMap.fromList $ mapIndex (\a b -> (b, a)) list

jump :: Int -> Int -> IntMap.IntMap Int -> Int
jump step jumpMark jumpList = case maybeJumpCommand of
  Nothing -> step
  Just x -> jump (step +1) (jumpMark + x) updatedJumpList
  where
    incrementExistingValue key existing = Just $ existing + 1
    lookupResult = IntMap.updateLookupWithKey incrementExistingValue jumpMark jumpList
    maybeJumpCommand = fst lookupResult
    updatedJumpList = snd lookupResult

mapIndex :: (a -> Int -> b) -> [a] -> [b]
mapIndex f l = zipWith f l [0..]