module Algorithm.OptimalBlocks.BuzzHash.Speed
( speed
) where

import Data.ByteString ( ByteString )

import Criterion.Main ( bgroup, bench, nf, Benchmark )

import Algorithm.OptimalBlocks.BuzzHash ( hashes, h )

speed :: ByteString -> [Benchmark]
speed bytes =
    [ bgroup "BuzzHash"
      [ bgroup "h" $
        [ bench (show n) (nf h n) | n <- 0:[7,15..255]]
      , bgroup "windows"
        [ bench (show n) $ nf (hashes n) bytes | n <- 64:[128, 256..2048]]
      ]
    ]

