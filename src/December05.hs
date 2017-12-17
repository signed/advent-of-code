module December05 where

import Data.Maybe
import qualified Data.IntMap as IntMap

stepsToExit :: [Int] -> Int
stepsToExit list = jump 0 0 initialJumpMap incrementExistingValue  where
  initialJumpMap = IntMap.fromList $ mapIndex (\a b -> (b, a)) list

stepsToExitPartTwo :: [Int] -> Int
stepsToExitPartTwo list = jump 0 0 initialJumpMap decrementIfGreaterThanThree where
  initialJumpMap = IntMap.fromList $ mapIndex (\a b -> (b, a)) list

jump :: Int -> Int -> IntMap.IntMap Int -> JumpAdjust -> Int
jump step jumpMark jumpList jumpAdjust = case maybeJumpCommand of
  Nothing -> step
  Just x -> jump (step +1) (jumpMark + x) updatedJumpList jumpAdjust
  where
    lookupResult = IntMap.updateLookupWithKey jumpAdjust jumpMark jumpList
    maybeJumpCommand = fst lookupResult
    updatedJumpList = snd lookupResult

type JumpAdjust = IntMap.Key -> Int -> Maybe Int

decrementIfGreaterThanThree:: JumpAdjust
decrementIfGreaterThanThree key existing
  | existing >= 3 = Just $ existing - 1
  | otherwise     = incrementExistingValue key existing

incrementExistingValue:: JumpAdjust
incrementExistingValue key existing = Just $ existing + 1

mapIndex :: (a -> Int -> b) -> [a] -> [b]
mapIndex f l = zipWith f l [0..]