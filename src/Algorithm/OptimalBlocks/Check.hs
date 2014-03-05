module Algorithm.OptimalBlocks.Check
( ArbByteString(..)
, ArbByteString1M(..)
, ArbByteString10M(..)
, fastRandBs
, slowRandBs
, check
) where

import qualified Data.ByteString as BS
import Data.ByteString ( ByteString )
import Crypto.Hash.Skein512 ( hash )
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property ( Result(reason), failed, succeeded )
import Test.QuickCheck.Gen
import Test.QuickCheck ( quickCheck )
import Data.Bits ( (.&.) )

import qualified Algorithm.OptimalBlocks.BuzzHash as BH
import Algorithm.OptimalBlocks ( Blocks(..), chop', sizedBitmask )

check :: IO ()
check = do
  quickCheck blocks
  where
  maxSz = 10*1024

  blocks :: ArbByteString1M -> Result
  blocks (ABS1M bs) =
    let mask  = sizedBitmask maxSz
        Blocks clean final = chop' 128 maxSz bs
        lengths = map BS.length clean
        goodLen = all (>= 128) lengths

        endings = [ BS.drop (BS.length b - 128) b | b <- clean ]
        hashes  = [ BH.currentVal $ BH.init b | b <- endings ]
        goodEnd = all (\h -> mask == h .&. mask) hashes

    in case (goodEnd, goodLen, bs == (BS.concat $ clean ++ [final])) of
        (False, _, _) -> failed { reason="Block with bad ending" }
        (_, False, _) -> failed { reason="Block too short" }
        (_, _, False) -> failed { reason="Reassembly failed" }
        _ -> succeeded

newtype ArbByteString = ABS ByteString deriving (Eq, Ord, Read, Show )
newtype ArbByteString1M = ABS1M ByteString deriving (Eq, Ord, Read, Show )
newtype ArbByteString10M = ABS10M ByteString deriving (Eq, Ord, Read, Show )

instance Arbitrary ArbByteString where
  arbitrary = do
    len <- choose (0, 100*1024)
    ABS `fmap` fastRandBs len

  shrink (ABS bs) = map ABS $ shrinks bs

instance Arbitrary ArbByteString1M where
  arbitrary =
    ABS1M `fmap` fastRandBs (11024*1024)

  shrink (ABS1M bs) = map ABS1M $ shrinks bs

instance Arbitrary ArbByteString10M where
  arbitrary =
    ABS10M `fmap` fastRandBs (10*1024*1024)

  shrink (ABS10M bs) = map ABS10M $ shrinks bs

fastRandBs :: Int -> Gen ByteString
fastRandBs len = do
  let skLen = 1024*1024
  let (rounds, bytes) = len `divMod` skLen
  bSeed <- slowRandBs $ 128 `div` 8
  -- Notice the hash (8*) calls; hash always returns an integral number of
  -- bytes (duh), but it wants its output length in bits. We just always track
  -- bytes, and multiply by 8 when calling hash.
  let tailBs = hash (8*bytes) bSeed
  if rounds == 0
    then return tailBs
    else do
      rSeed <- slowRandBs $ 128 `div` 8
      return $ BS.concat $ tailBs : take rounds (tail $ iterate (hash $ 8*skLen) rSeed)

slowRandBs :: Int -> Gen ByteString
slowRandBs numBytes = BS.pack `fmap` vectorOf numBytes (choose (0, 255))

shrinks :: ByteString -> [ByteString]
shrinks bs =
  [ BS.append a b | (a, b) <- zip (BS.inits bs) (tail $ BS.tails bs) ]

