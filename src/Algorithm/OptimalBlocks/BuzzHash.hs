{-# LANGUAGE BangPatterns #-}
{- | This is a rolling hash used to calculate the hashes of fixed-length
 sequences of characters within a given ByteString. BuzzHash is nice in that it
 makes this calculation very efficient.
 -}

module Algorithm.OptimalBlocks.BuzzHash
( hashes
, split
, slowSplit
, Hash(..)
, init
, roll
, h
) where

import qualified Data.ByteString as ByteString
import qualified Data.Vector.Unboxed as VU
import Data.Bits              ( (.&.), rotateL, shiftL, xor )
import Data.ByteString        ( ByteString, pack, length )
import Data.ByteString.Unsafe ( unsafeTake, unsafeIndex )
import Data.Vector.Unboxed    ( Vector, generate, (!), empty, constructN, unsafeLast, null )
import Data.Word              ( Word8, Word64 )

import Algorithm.OptimalBlocks.SipHash ( hashByteString )

import Prelude hiding ( init, length, null, rem )

type WindowSize = Int
type BlockShift = Int

{- Split up a ByteString into a complete block and a remainder. If no break
 - point is found in the given string, the first element of the resulting pair
 - will be empty.
 -}
split :: WindowSize -> BlockShift -> ByteString -> (ByteString, ByteString)
split win _shift bytes | ByteString.length bytes <= win =
  (ByteString.empty, bytes)
split win shift bytes = go 0 (hash0 $ unsafeTake win bytes)
  where
  go !start !hash | mask == (hash .&. mask) =
    ByteString.splitAt (start + win) bytes
  go !start !_hash | start == end =
    (ByteString.empty, bytes)
  go !start !hash =
    -- 8-byte window:
    -- hash of 0 1 2 3 4 5 6 7
    -- rolls into
    -- hash of   1 2 3 4 5 6 7 8
    -- by removing the value at "start" and adding in the value at "start+win"
    let !old = unsafeIndex bytes start
        !new = unsafeIndex bytes (start + win)
        !hh' = roll win hash old new
    in go (start+1) hh'

  -- 8-byte window, 10 bytes of data, rolls like
  -- [01234567]89  (start = 0)
  -- 0[12345678]9  (start = 1)
  -- 01[23456789]  (start = 2)
  -- we are done when start = (length bytes) - win (2, here)
  end = ByteString.length bytes - win

  mask = (1 `shiftL` shift) - 1

{- Same thing as 'split', but actually calculates the hash of every window
 - explicitly. This is really slow, and is only used to validate the results of
 - 'split'
 -}
slowSplit :: WindowSize -> BlockShift -> ByteString -> (ByteString, ByteString)
slowSplit win _shift bytes | ByteString.length bytes <= win =
  (ByteString.empty, bytes)
slowSplit win shift bytes = go 0
  where
  go start =
    let tl   = ByteString.drop start bytes
        bs   = ByteString.take win tl
        hash = hash0 bs
    in if ByteString.length bs < win
        then (ByteString.empty, bytes)
        else if mask == (mask .&. hash)
              then ByteString.splitAt (start + win) bytes
              else go (start+1)

  mask = (1 `shiftL` shift) - 1

{-| Determine the hash of every 'len' sequence of bytes in the given
 'ByteString'. Because this uses BuzzHash, a rolling hash, this runs in @O(n)@
 time dependent on the length of 'bs', and independent of 'len'.

 This will generate @(length bs - len + 1)@ 64-bit hashes.

 This function really doesn't serve any purpose anymore, and is just kept
 around as a sanity check for the much faster 'split' function.
 -}
hashes :: WindowSize  -- ^ How many bytes to put into each hash
       -> ByteString  -- ^ The 'ByteString' to calculate hashes of.
       -> Vector Word64
hashes len bs 
  | length bs < len = empty
  | otherwise = constructN (length bs - len + 1) upd
  where
  upd :: Vector Word64 -> Word64
  upd v
    | null v    = currentVal $ init $ unsafeTake len bs
    | otherwise =
        let !prevH = {-# SCC hash_last #-} Hash len $ unsafeLast v
            !vlen  = VU.length v - 1
            !old8  = unsafeIndex bs vlen
            !new8  = unsafeIndex bs (len + vlen)
            !rollV = roll (windowLength prevH) (currentVal prevH) old8 new8
        in rollV

{-| A representation of a hash that allows rolling hashes to be easily
 calculated.
 -}
data Hash = Hash { windowLength :: !WindowSize
                 , currentVal   :: !Word64
                 }
            deriving ( Show )

{-| Create a 'Hash' instance using an entire 'ByteString'. This doesn't have
 any sort of length argument to do partial 'ByteString's because 'ByteString'
 supports efficient slices on its own.
 -}
init :: ByteString -> Hash
init bs = Hash (length bs) (hash0 bs)

{-| Calculate the actual hash of the given ByteString.
 -}
hash0 :: ByteString -> Word64
hash0 bs = ByteString.foldl upd 0 bs
  where
  upd :: Word64 -> Word8 -> Word64
  upd hsh w8 = (hsh `rotateL` 1) `xor` (h w8)

{-| Roll the 'Hash' to the next byte over in the 'ByteString' being hashed.
 This doesn't do any sort of checking to ensure that 'old' and 'new' are
 actually correct, so this is probably easy to mess up when using it manually.
 The expected usage is that one would initialize a hash using 'init' on the
 beginning of some 'ByteString', and then to calculate the hash of each
 sequence of bytes one would manually track the first and last byte of each
 window on the 'ByteString'. 'hashes' does this for you...
 -}
roll :: WindowSize -> Word64 -> Word8 -> Word8 -> Word64
roll !winsz !hsh !old !new =
  let !rem = {- {-# SCC roll_rem #-} -} (h old) `rotateL` winsz
      !add = {- {-# SCC roll_add #-} -} h new
      !skh = {- {-# SCC roll_skh #-} -} hsh `rotateL` 1
  in {-# SCC roll_hsh #-} rem `xor` add `xor` skh
{-# INLINE roll #-}

{-| Upgrade an 8-bit word to a 64-bit one that is very "different" from all
 the other possible 8-bit words. This library uses SipHash to do this.
 -}
h :: Word8 -> Word64
h = (hs !) . fromEnum
{-# INLINE h #-}

{- |A pre-calculated array of hashes of bytes.
 -}
hs :: Vector Word64
hs = generate 256 fn
  where
  fn n = hashByteString 2 4 k0 k1 $ pack [toEnum n]
  k0 = 0x4a7330fae70f52e8
  k1 = 0x919ea5953a9a1ec9
{-# NOINLINE hs #-}
