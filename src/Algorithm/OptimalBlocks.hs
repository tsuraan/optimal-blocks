{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 708
{-# OPTIONS_GHC -fno-spec-constr #-}
#endif
#endif
module Algorithm.OptimalBlocks
( chop
, chop'
, sizedBitmask
, Blocks(..)
) where

import qualified Data.Vector.Unboxed as V
import Data.ByteString ( ByteString, length, splitAt)
import Data.Word ( Word64 )
import Data.Bits ( (.&.), shiftL )
import Control.DeepSeq ( NFData(..) )

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
  | length bs < winSz = Blocks [] bs
  | otherwise = go
  where
  go =
    let hashed = hashes winSz bs
        locs   = V.map (+winSz) $ V.findIndices (\h -> mask == (mask .&. h))
                                                hashed
        lens   = V.zipWith (-) locs (V.cons 0 locs)
        (end, rlist, _) = V.foldl' split (bs, [], 0) lens
    in Blocks (reverse rlist) end

  mask :: Word64
  mask = sizedBitmask maxSz

  -- Split is a little bit complicated. The goal here is that split will never
  -- give a chunk of data smaller than winSz or larger than maxSz. The reason
  -- for the lower bound is that we're scanning for winSz-length chunks of data
  -- whose hashes meet a pattern; if data chunks smaller than winSz are
  -- returned, they don't have well-defined winSz-sized hashes, and we don't
  -- want that. The reason for the upper bound is pretty obvious. So, split is
  -- essentially given a sequence of distances between bytestring indices; if
  -- the distance is too small, it will save it for the next time it's called.
  -- Once it's been called on enough small distances (enough to be >= winSz),
  -- then it will actually do a split. If it's always given really large
  -- distances, then it will just do splits all the time.
  split :: (ByteString, [ByteString], Int)
        -> Int
        -> (ByteString, [ByteString], Int)
  split (b, ls, add) loc
    | add+loc < winSz = (b, ls, add+loc)
    | otherwise =
        let (h, t) = splitAt (add+loc) b
        in (t, h:ls, 0)

sizedBitmask :: Int -> Word64
sizedBitmask maxSz =
  let target = toEnum   $ maxSz `div` 2 :: Float
      bits   = fromEnum $ 0.5 + logBase 2 target
  in 1 `shiftL` bits - 1

instance NFData Blocks where
  rnf (Blocks lst b) =
   b `seq` examine lst
   where
   examine [] = ()
   examine (hd:tl) = hd `seq` examine tl

