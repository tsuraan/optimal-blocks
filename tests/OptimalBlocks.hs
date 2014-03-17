module OptimalBlocks
( check
) where

import qualified Data.ByteString as BS
import Test.QuickCheck.Property ( Result(reason), failed, succeeded )
import Test.QuickCheck ( quickCheck )
import Data.Bits ( (.&.) )

import Data.ByteString.Arbitrary ( ArbByteString1M(..) )

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

