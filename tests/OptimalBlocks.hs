module OptimalBlocks
( check
) where

import qualified Data.ByteString as BS
import Test.QuickCheck.Property ( Result(reason), failed, succeeded )
import Test.QuickCheck ( quickCheck )
import Data.Bits ( (.&.) )
import Data.Word ( Word16 )

import Data.ByteString.Arbitrary ( ArbByteString1M(..) )

import qualified Algorithm.OptimalBlocks.BuzzHash as BH
import Algorithm.OptimalBlocks

check :: IO ()
check = do
  quickCheck blocks
  where
  maxSz = 10*1024

  blocks :: Word16 -> Word16 -> ArbByteString1M -> Result
  blocks doOld doRef (ABS1M bs) =
    let doOld' = asPercent doOld > 0.9
        doRef' = asPercent doRef > 0.95
        mask   = sizedBitmask maxSz
        cfg    = defaultConfig{blockSize=maxSz}
        funs   = [ chop (cfg{chunkAlg=New}) bs ] ++
                 [ chop (cfg{chunkAlg=Old}) bs | doOld' ] ++
                 [ chop (cfg{chunkAlg=Reference}) bs | doRef' ]

        Blocks optimalB final = head funs
        optimalBS = [ b | OptimalBlock b <- optimalB ]
        lengths   = map BS.length optimalBS
        goodLen   = all (>= 128) lengths

        endings   = [ BS.drop (BS.length b - 128) b | b <- optimalBS ]
        hashes    = [ BH.currentVal $ BH.init b | b <- endings ]
        goodEnd   = all (\h -> mask == h .&. mask) hashes
        equals    = all (== head funs) funs

    in case (equals, goodEnd, goodLen, bs == (BS.concat $ optimalBS ++ [final])) of
        (False, _, _, _) -> failed { reason="Different results with different impls" }
        (_, False, _, _) -> failed { reason="Block with bad ending" }
        (_, _, False, _) -> failed { reason="Block too short" }
        (_, _, _, False) -> failed { reason="Reassembly failed" }
        _ -> succeeded

  asPercent :: Word16 -> Float
  asPercent w16 =
    let top = toEnum $ fromEnum w16
        bot = toEnum $ fromEnum (maxBound :: Word16)
    in top / bot
        
