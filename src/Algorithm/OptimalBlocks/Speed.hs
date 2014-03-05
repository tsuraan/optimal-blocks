module Algorithm.OptimalBlocks.Speed
( speed
) where

import Data.ByteString ( ByteString )
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Criterion.Main ( Benchmark, bgroup, bench, nf )

import Algorithm.OptimalBlocks.Check ( ArbByteString(..), fastRandBs, slowRandBs )
import Algorithm.OptimalBlocks ( chop )

speed :: ByteString -> [Benchmark]
speed bytes =
    [ bgroup "ArbByteString"
      [ bench "arbitrary" $ action (arbitrary :: Gen ArbByteString)
      , bgroup "fastRandBs"
        [ bench "10B" $ action (fastRandBs 10)
        , bench "1KB" $ action (fastRandBs 1024)
        , bench "1MB" $ action (fastRandBs (1024*1024))
        ]
      , bgroup "slowRandBs"
        [ bench "10B" $ action (slowRandBs 10)
        , bench "1KB" $ action (slowRandBs 1024)
        , bench "10KB" $ action (slowRandBs 10240)
        ]
      ]
    , bench "chop" $ nf chop bytes
    ]

  where
  action :: Gen a -> IO ()
  action act = do
    samples <- sample' act
    examine samples
    where
    examine :: [a] -> IO ()
    examine [] = return ()
    examine (hd:tl) = hd `seq` examine tl
