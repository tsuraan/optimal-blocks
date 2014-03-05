module Main
( main
) where

import qualified Algorithm.OptimalBlocks.BuzzHash.Speed as Buzz
import qualified Algorithm.OptimalBlocks.Speed as OB
import qualified Data.ByteString as BS
import System.IO ( withFile, IOMode(ReadMode) )
import Criterion.Main ( defaultMain )

main :: IO ()
main = do
  bytes <- withFile "/dev/urandom" ReadMode $ (flip BS.hGet) (10 * 1024 * 1024)
  defaultMain $ (OB.speed bytes) ++ (Buzz.speed bytes)

