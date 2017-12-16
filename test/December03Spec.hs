module December03Spec where
import Test.Hspec
import Data.Char(digitToInt)
import December03(distanceToCenter,spiralRing, hmm, Coordinates(..), manhattenDistance)

spec :: Spec
spec = do
  describe "inner spiral" $ do
    it "1 is in ring 0" $
       spiralRing 1 `shouldBe` 0
    it "8 is in the 1 ring" $
       spiralRing 8 `shouldBe` 1
    it "9 is in the 1 ring" $
       spiralRing 9 `shouldBe` 1
    it "10 is in the 2 ring" $
       spiralRing 10 `shouldBe` 2
    it "25 is in the 2 ring" $
       spiralRing 25 `shouldBe` 2
    it "26 is in the 3 ring" $
       spiralRing 26 `shouldBe` 3

  describe "distance" $ do
    it "same point" $
      manhattenDistance (D2 0 0) (D2 0 0) `shouldBe` 0
    it "flup" $
      manhattenDistance (D2 0 0) (D2 (-15) 16) `shouldBe` 31

  describe "hmm" $ do
    it "1 2" $
       hmm 1 2 `shouldBe` D2 1 0
    it "1 3" $
       hmm 1 3 `shouldBe` D2 1 1
    it "1 4" $
       hmm 1 4 `shouldBe` D2 0 1
    it "1 5" $
       hmm 1 5 `shouldBe` D2 (-1) 1
    it "1 6" $
       hmm 1 6 `shouldBe` D2 (-1) 0
    it "1 7" $
       hmm 1 7 `shouldBe` D2 (-1) (-1)
    it "1 8" $
       hmm 1 8 `shouldBe` D2 0 (-1)
    it "1 9" $
       hmm 1 9 `shouldBe` D2 1 (-1)
    it "2 10" $
       hmm 2 10 `shouldBe` D2 2 (-1)
    it "2 11" $
       hmm 2 11 `shouldBe` D2 2 0
    it "2 15" $
       hmm 2 15 `shouldBe` D2 0 2
    it "2 23" $
       hmm 2 23 `shouldBe` D2 0 (-2)
    it "2 25" $
       hmm 2 25 `shouldBe` D2 2 (-2)

  describe "distanceToCenter" $ do
    it "distance to center is 0" $
       distanceToCenter 1 `shouldBe` 0
    it "acceptance two" $
       distanceToCenter 12 `shouldBe` 3
    it "acceptance three" $
       distanceToCenter 23 `shouldBe` 2
    it "acceptance four" $
       distanceToCenter 1024 `shouldBe` 31
    it "challenge" $
       distanceToCenter 347991 `shouldBe` 480

