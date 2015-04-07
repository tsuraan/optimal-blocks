{-# LANGUAGE BangPatterns #-}
module Algorithm.OptimalBlocks.Buzz2
( split
, slowSplit
) where

import Algorithm.OptimalBlocks.SipHash ( hashByteString )

import qualified Data.ByteString as ByteString

import Data.Bits              ( (.&.), rotateL, shiftL, xor )
import Data.ByteString        ( ByteString, pack )
import Data.ByteString.Unsafe ( unsafeIndex, unsafeTake )
import Data.Vector.Unboxed    ( Vector, generate, (!) )
import Data.Word              ( Word8, Word64 )

-- import Debug.Trace ( trace )

import Prelude hiding ( rem )

type WindowSize = Int
type BlockShift = Int

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
        !rem = (h old) `rotateL` win
        !add = h new
        !skh = hash `rotateL` 1
        !hh' = rem `xor` add `xor` skh
    in go (start+1) hh'

  -- 8-byte window, 10 bytes of data, rolls like
  -- [01234567]89  (start = 0)
  -- 0[12345678]9  (start = 1)
  -- 01[23456789]  (start = 2)
  -- we are done when start = (length bytes) - win (2, here)
  end = ByteString.length bytes - win

  mask = (1 `shiftL` shift) - 1

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

hash0 :: ByteString -> Word64
hash0 bs = ByteString.foldl upd 0 bs
  where
  upd :: Word64 -> Word8 -> Word64
  upd hsh w8 = (hsh `rotateL` 1) `xor` (h w8)

h :: Word8 -> Word64
h = (hs !) . fromEnum
{-# INLINE h #-}

hs :: Vector Word64
hs = generate 256 fn
  where
  fn n = hashByteString 2 4 k0 k1 $ pack [toEnum n]
  k0 = 0x4a7330fae70f52e8
  k1 = 0x919ea5953a9a1ec9
{-# NOINLINE hs #-}
