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
, OptimalBlock(..)
) where

import qualified Data.Vector.Unboxed as V
import Data.ByteString ( ByteString, length, splitAt)
import Data.Word ( Word64 )
import Data.Bits ( (.&.), shiftL )
import Control.DeepSeq ( NFData(..) )

import Algorithm.OptimalBlocks.BuzzHash ( hashes )

import Prelude hiding ( length, splitAt )

-- | Alias for 'ByteString', used to indicate that this sequence of bytes ends
-- in an optimal fashion.
newtype OptimalBlock = OptimalBlock
                       { fromOptimal :: ByteString
                       } deriving ( Eq, Ord, Show )

-- | The result of the 'chop'' function, contains the list of optimal blocks
-- that were found, and any remaining bytes that did not end optimally.
data Blocks = Blocks 
              { blocksOptimal :: [OptimalBlock]
              , blocksRemain  :: ByteString
              } deriving ( Show )

{-| This is an alias of 'chop'' that uses a window size of 128 bytes and a
 desired block size of 2MB.
 -}
chop :: ByteString -> Blocks
chop = chop' 128 (2 * mb)
  where
  mb = 1024*1024

{-| Chop up a 'ByteString' into blocks of data that are likely to occur in
 other 'ByteString's. This uses roughly the same algorithm that rsync does:
 calculate a hash of every 'winSz'-sized sequence of bytes within the given
 'ByteString', and then break it up where the hashes match a certain pattern.
 Specifically, this function uses BuzzHash (a rolling hash) to make the hash
 calculations fast, and the pattern it looks for is that the hash's binary form
 ends with the right number of "ones", where "right" is determined by the given
 'desiredSz'. The breaks are inserted after the matching windows are found.
 -}
chop' :: Int        -- ^ Window size for rolling hash
      -> Int        -- ^ Desired average block size
      -> ByteString -- ^ ByteString to chop
      -> Blocks
chop' winSz desiredSz bs
  | length bs < winSz = Blocks [] bs
  | otherwise = go
  where
  go =
    let hashed = hashes winSz bs
        locs   = V.map (+winSz) $ V.findIndices (\h -> mask == (mask .&. h))
                                                hashed
        lens   = V.zipWith (-) locs (V.cons 0 locs)
        (end, rlist, _) = V.foldl' split (bs, [], 0) lens
    in Blocks (map OptimalBlock $ reverse rlist) end

  mask :: Word64
  mask = sizedBitmask desiredSz

  -- Split is a little bit complicated. The goal here is that split will never
  -- give a chunk of data smaller than winSz. The reason for this is that we're
  -- scanning for winSz-length chunks of data whose hashes meet a pattern; if
  -- data chunks smaller than winSz are returned, they don't have well-defined
  -- winSz-sized hashes, and we don't want that.
  split :: (ByteString, [ByteString], Int)
        -> Int
        -> (ByteString, [ByteString], Int)
  split (b, ls, add) loc
    | add+loc < winSz = (b, ls, add+loc)
    | otherwise =
        let (h, t) = splitAt (add+loc) b
        in (t, h:ls, 0)

-- | Determine the bitmask that will probably give us blocks of size
-- 'desiredSz'. The idea behind this is that if, for example, we want 1MB
-- blocks, then we need a bitmask that will match one window in (1024*1024).
-- This is equivalent to saying that we want the hash's bottom 20 bits to be
-- set (a 1 in 2**20 occurrance). This function's ugly, and uses logarithms and
-- lots of type conversions, but it's only called once per 'chop'' call, so it
-- doesn't have much impact on performance.
sizedBitmask :: Int -> Word64
sizedBitmask desiredSz =
  let target = toEnum   $ desiredSz `div` 2 :: Float
      bits   = fromEnum $ 0.5 + logBase 2 target
  in 1 `shiftL` bits - 1

instance NFData Blocks where
  rnf (Blocks lst b) =
   b `seq` examine lst
   where
   examine [] = ()
   examine (hd:tl) = hd `seq` examine tl

