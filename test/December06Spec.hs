module December06Spec where
import Test.Hspec
import December06

--2	8	8	5	4	2	3	1	5	5	1	2	15	13	5	14
challenge = [2,8,8,5,4,2,3,1,5,5,1,2,15,13,5,14]

memoryBanks :: [Int] -> [MemoryBank]
memoryBanks xs  = map MemoryBank xs

spec :: Spec
spec =
  describe "redistribute" $ do
    it "challenge" $
       redistributionCycleTillAlreadySeenConfiguration (memoryBanks challenge) `shouldBe` 3156
    it "challenge part two" $
       cyclesBetweenRepetitions (memoryBanks challenge) `shouldBe` 1610
    it "acceptance" $ do
       let banks = memoryBanks[0, 2, 7, 0]
       redistributionCycleTillAlreadySeenConfiguration banks `shouldBe` 5
    it "acceptance part two" $ do
       let banks = memoryBanks[0, 2, 7, 0]
       cyclesBetweenRepetitions banks `shouldBe` 4
