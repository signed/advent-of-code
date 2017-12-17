module December04 where
import Data.List(group, sort)

validPassPhraseCount :: [String] -> Int
validPassPhraseCount x = length $ filter validPassPhrase x

validPassPhrase :: String -> Bool
validPassPhrase x = null clustersWithSizeGreaterOne  where
  groupedWords = group $ sort $  words x
  groupSizes = map length groupedWords
  clustersWithSizeGreaterOne = filter sameWordMoreThanOnce groupSizes
  sameWordMoreThanOnce x = x > 1
