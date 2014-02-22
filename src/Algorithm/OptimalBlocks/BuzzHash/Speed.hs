module Algorithm.OptimalBlocks.BuzzHash.Speed
( speed
) where

import qualified Data.ByteString as BS
import System.IO ( withFile, IOMode(ReadMode) )

import Criterion.Main ( defaultMain, bgroup, bench, nf )

import Algorithm.OptimalBlocks.BuzzHash ( hashes, h )

speed :: IO ()
speed = do
  bytes <- withFile "/dev/urandom" ReadMode $ (flip BS.hGet) (10 * 1024 * 1024)
  defaultMain
    [ bgroup "BuzzHash"
      [ bgroup "h" $
        [ bench "0" (nf h 0) ] ++ [
          bench (show n) (nf h n) | n <- [7,15..255]]
      , bgroup "windows"
        [ bench (show n) $ nf (hashes n) bytes | n <- 64:[128, 256..2048]]
      ]
    ]

