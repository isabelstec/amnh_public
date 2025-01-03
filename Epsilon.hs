module Epsilon (compareWithEpsilon) where

-- | Compare two doubles with an epsilon of 1%.
compareWithEpsilon :: Double -> Double -> Bool
compareWithEpsilon x y =
    let epsilon = 0.01
        diff = abs (x - y)
        smaller = min x y
    in diff < (epsilon * smaller)