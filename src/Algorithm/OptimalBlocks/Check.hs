module Algorithm.OptimalBlocks.Check
( ArbByteString(..)
) where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Data.ByteString ( ByteString, append, inits, tails, pack )

newtype ArbByteString = ABS ByteString deriving (Eq, Ord, Read, Show )

instance Arbitrary ArbByteString where
  arbitrary = do
    len <- choose (0, 30*1024)
    letters <- vectorOf len $ choose (0, 255)
    return $ ABS $ pack letters

  shrink (ABS bs) =
    [ ABS $ append a b | (a, b) <- zip (inits bs) (tail $ tails bs) ]
