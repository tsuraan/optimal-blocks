module OptimalBlocks
( speed
) where

import Data.ByteString ( ByteString )
import Criterion.Main ( Benchmark, bench, nf )

import Algorithm.OptimalBlocks ( chop, defaultConfig )

speed :: ByteString -> [Benchmark]
speed bytes =
    [ bench "chop" $ nf (chop defaultConfig) bytes
    ]

