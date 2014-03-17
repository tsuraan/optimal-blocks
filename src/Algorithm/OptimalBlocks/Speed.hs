module Algorithm.OptimalBlocks.Speed
( speed
) where

import Data.ByteString ( ByteString )
import Criterion.Main ( Benchmark, bench, nf )

import Algorithm.OptimalBlocks ( chop )

speed :: ByteString -> [Benchmark]
speed bytes =
    [ bench "chop" $ nf chop bytes
    ]

