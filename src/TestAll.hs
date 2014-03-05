module Main
( main
) where

import qualified Algorithm.OptimalBlocks.BuzzHash.Check as Buzz
import Algorithm.OptimalBlocks.Check as Optimal

main :: IO ()
main = do
  putStrLn "Checking BuzzHash Algorithms"
  Buzz.check
  putStrLn "Checking Optimal Block Algorithms"
  Optimal.check

