module December03 where

data Coordinates = D2 Int Int deriving (Eq, Show)
edgeLength :: Int -> Int
edgeLength x = x * 2 +1

manhattenDistance :: Coordinates -> Coordinates -> Int
manhattenDistance x@(D2 x1 y1) y@(D2 x2 y2) = xDistance + yDistance where
  xDistance = abs (x1 - x2)
  yDistance = abs (y1 - y2)

spiralRing :: Int -> Int
spiralRing 1 = 0
spiralRing k  =  jup 1 candidates where
                  candidates = [n * 2 + 1 | n <- [0..]]
                  jup :: Int -> [Int] -> Int
                  jup ring (x : y : xs) =  if y*y >= k then ring else jup (ring + 1) (y:xs)

hmm :: Int -> Int -> Coordinates
hmm ring total
--  | True = D2 yStart missing
  | missing < rightMax  = D2 xStart (yStart + missing -1)
  | missing <= topMax   = D2 (halfEdgeLength - (missing - rightMax)) halfEdgeLength
  | missing <= leftMax  = D2 (-halfEdgeLength) (halfEdgeLength - (missing - topMax))
  | otherwise           = D2 (-halfEdgeLength + (missing - leftMax)) (-halfEdgeLength)
  where
         edgeLengthI = ring  * 2 + 1
         consumedOnEachSide = edgeLengthI - 1
         innerEdgeLength = edgeLength (ring - 1)
         missing = total - innerEdgeLength * innerEdgeLength
         rightMax = consumedOnEachSide
         topMax = rightMax + consumedOnEachSide
         leftMax = topMax + consumedOnEachSide
         halfEdgeLength = quot edgeLengthI 2
         xStart = ring
         yStart = 1 - ring

distanceToCenter :: Int -> Int
distanceToCenter x = manhattenDistance (D2 0 0) $ hmm ring x where
                      ring = spiralRing x