import Control.DeepSeq (NFData)
import Control.Parallel.Strategies (parMap, rdeepseq)
import System.Random (mkStdGen, randoms)

import Criterion (bench, nf)
import Criterion.Main (defaultMain)

-- Merge two lists in O(n).
merge :: Ord a => [a] -> [a] -> [a]
merge []       ys       = ys
merge xs       []       = xs
merge (x : xs) (y : ys)
    | x < y             = x : merge xs (y : ys)
    | otherwise         = y : merge (x : xs) ys

-- | Split a list in O(n).
split :: [a] -> ([a], [a])
split = go [] []
  where
    go xs ys (x : y : zs) = go (x : xs) (y : ys) zs
    go xs ys zs           = (xs, zs ++ ys)

-- | Merge sort in O(n * log n)
sort :: Ord a => [a] -> [a]
sort []  = []
sort [x] = [x]
sort ls  =
    let (xs, ys) = split ls
        xs'      = sort xs
        ys'      = sort ys
    in merge xs' ys'

sortPar :: (NFData a, Ord a) => Int -> [a] -> [a]
sortPar _     []  = []
sortPar _     [x] = [x]
sortPar elems ls  =
    let rec      = if elems <= 4000 then sort else sortPar $ elems `div` 2
        (xs, ys) = split ls
    in concat $ parMap rdeepseq rec [xs, ys]

main :: IO ()
main = defaultMain
    [ bench "sort"    $ nf sort input
    , bench "sortPar" $ nf (sortPar $ length input) input
    ]
  where
    stdGen = mkStdGen 0
    input  = take 20000 $ randoms stdGen :: [Int]
    {-# NOINLINE input #-}
