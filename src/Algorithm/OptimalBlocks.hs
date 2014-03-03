{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 708
{-# OPTIONS_GHC -fno-spec-constr #-}
#endif
#endif
module Algorithm.OptimalBlocks
( chop
, Blocks(..)
) where

import qualified Data.Vector.Unboxed as V
import Data.ByteString ( ByteString, length, splitAt)
import Data.Word ( Word64 )
import Data.Bits ( (.&.), shiftL )

import Algorithm.OptimalBlocks.BuzzHash ( hashes )

import Prelude hiding ( length, splitAt )

data Blocks = Blocks [ByteString] ByteString
           deriving ( Show )

chop :: ByteString -> Blocks
chop = chop' 128 (2 * mb)
  where
  mb = 1024*1024

chop' :: Int        -- ^ Window size for rolling hash
      -> Int        -- ^ Max allowed block size
      -> ByteString -- ^ ByteString to chop
      -> Blocks
chop' winSz maxSz bs
  | length bs < maxSz = Blocks [] bs
  | otherwise = go
  where
  go =
    let hashed = hashes winSz bs
        locs   = V.map (+winSz) $ V.findIndices (\h -> mask == (mask .&. h))
                                                hashed
        -- Notice that this filter is totally wrong; I need to acumulate
        -- differences that are too small, not delete them. Fix this once the
        -- code no longer gives GHC fits...
        lens   = V.filter (>=winSz) $ V.zipWith (-) locs (V.cons 0 locs)
        (end, rlist) = V.foldl' split (bs, []) lens
    in Blocks (reverse rlist) end

  mask :: Word64
  mask =
    let target = toEnum   $ maxSz `div` 2 :: Float
        bits   = fromEnum $ 0.5 + logBase 2 target
    in 1 `shiftL` bits - 1

  split :: (ByteString, [ByteString]) -> Int -> (ByteString, [ByteString])
  split (b, ls) loc =
    let (h, t) = splitAt loc b
    in if loc <= maxSz then (t, h:ls)
                       else (t, splitHarder h ++ ls)

  splitHarder :: ByteString -> [ByteString]
  splitHarder b 
    | length b <= maxSz = [b]
    | otherwise =
        let (h, t) = splitAt maxSz b
        in h : splitHarder t
