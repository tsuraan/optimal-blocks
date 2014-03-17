module Algorithm.OptimalBlocks.BuzzHash.Check
( check
) where

import qualified Data.ByteString as BS

import Data.Vector.Unboxed ( fromList )
import Data.ByteString.Arbitrary ( ArbByteString(ABS) )
import Test.QuickCheck ( quickCheck )
import Test.QuickCheck.Gen ( choose )

import Algorithm.OptimalBlocks.BuzzHash ( Hash(currentVal), hashes, init )

import Prelude hiding ( init )

check :: IO ()
check = quickCheck go
  where
  go (ABS bs) = do
    winLen <- choose (1, BS.length bs)
    let hashes1 = hashes winLen bs
    let hashes2 = fromList [ currentVal $ init $ BS.take winLen $ BS.drop n bs
                           | n <- [0..BS.length bs - winLen]]
    return $ hashes1 == hashes2

