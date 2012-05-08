{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Parallel (par, pseq)

import Criterion (bench, whnf)
import Criterion.Main (defaultMain)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

numLines :: ByteString -> Int
numLines = BC.count '\n'
{-# INLINE numLines #-}

numLinesPar :: ByteString -> Int
numLinesPar bs
    | len < 16384 = numLines bs
    | otherwise   =
        let (bs1, bs2) = B.splitAt (len `div` 2) bs
            (ls1, ls2) = (numLinesPar bs1, numLinesPar bs2)
        in ls1 `par` ls2 `pseq` ls1 + ls2
  where
    len = B.length bs

main :: IO ()
main = defaultMain
    [ bench "numLines"    $ whnf numLines    input
    , bench "numLinesPar" $ whnf numLinesPar input
    ]
  where
    input = B.concat $ replicate 2000000 "Hello, world!\n"
    {-# NOINLINE input #-}
