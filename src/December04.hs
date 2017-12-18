module December04 where
import Data.List(group, sort)

type PassPhraseTester = [String] -> Bool

validPassPhraseCount :: [String] -> Int
validPassPhraseCount x = length $ filter ( validPassPhrase [noDuplicateWords] ) x

validPassPhraseCountTaskTwo :: [String] -> Int
validPassPhraseCountTaskTwo x = length $ filter (validPassPhrase [noDuplicateWords, noAnagramsOfAnyWord]) x

validPassPhrase :: [PassPhraseTester] -> String ->  Bool
validPassPhrase predicates passPhrase = all (\ f -> f split) predicates
 where split = words passPhrase

noDuplicateWords :: PassPhraseTester
noDuplicateWords words = null clustersWithSizeGreaterOne  where
  groupedWords = group $ sort words
  groupSizes = map length groupedWords
  clustersWithSizeGreaterOne = filter sameWordMoreThanOnce groupSizes
  sameWordMoreThanOnce x = x > 1

noAnagramsOfAnyWord :: PassPhraseTester
noAnagramsOfAnyWord words = True `notElem` booleans
 where booleans = map hmm words
       hmm word = reverse word `elem` words `without` word

without :: Eq a => [a] -> a -> [a]
without xs x = filter (x /=) xs

withoutElementAt :: Int -> [a] -> (a, [a])
withoutElementAt index list =
 let (front, toRemove:end) = splitAt index list
 in (toRemove, front ++ end)