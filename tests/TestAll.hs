module Main
( main
) where

import qualified Buzz
import qualified OptimalBlocks

main :: IO ()
main = do
  putStrLn "Checking BuzzHash Algorithms"
  Buzz.check
  putStrLn "Checking Optimal Block Algorithms"
  OptimalBlocks.check

