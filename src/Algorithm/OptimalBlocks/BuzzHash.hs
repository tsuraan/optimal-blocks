{-# LANGUAGE BangPatterns #-}
{- | This is a rolling hash used to calculate the hashes of fixed-length
 sequences of characters within a given ByteString. BuzzHash is nice in that it
 makes this calculation very efficient.
 -}

module Algorithm.OptimalBlocks.BuzzHash
( hashes
, Hash(..)
, init
, roll
, h
) where

import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed ( Vector, generate, (!), empty, constructN, unsafeLast, null )
import Data.ByteString ( ByteString, pack, length )
import Data.ByteString.Unsafe ( unsafeTake, unsafeIndex )
import Data.Word ( Word8, Word64 )
import Data.Bits ( rotateL, xor )

import Algorithm.OptimalBlocks.SipHash ( hashByteString )

import Prelude hiding ( init, length, null, rem )

hashes :: Int -> ByteString -> Vector Word64
hashes len bs
  | length bs < len = empty
  | otherwise = constructN (length bs - len + 1) upd
  where
  upd :: Vector Word64 -> Word64
  upd v
    | null v    = currentVal $ init $ unsafeTake len bs
    | otherwise =
        let prevH = Hash len $ unsafeLast v
            old8  = unsafeIndex bs (VU.length v - 1)
            new8  = unsafeIndex bs (len + VU.length v - 1)
        in currentVal $ roll prevH old8 new8

data Hash = Hash { windowLength :: !Int
                 , currentVal   :: !Word64
                 }
            deriving ( Show )

init :: ByteString -> Hash
init bs =
  let hash = BS.foldl upd 0 bs
  in Hash (length bs) hash
  where
  upd :: Word64 -> Word8 -> Word64
  upd hsh w8 = (hsh `rotateL` 1) `xor` (h w8)

roll :: Hash -> Word8 -> Word8 -> Hash
roll hsh old new =
  let rem = (h old) `rotateL` (windowLength hsh)
      add = h new
      skh = (currentVal hsh) `rotateL` 1
  in hsh { currentVal = rem `xor` add `xor` skh }
{-# INLINE roll #-}

h :: Word8 -> Word64
h = (hs !) . fromEnum

hs :: Vector Word64
hs = generate 256 fn
  where
  fn n = hashByteString 2 4 k0 k1 $ pack [toEnum n]
  k0 = 0x4a7330fae70f52e8
  k1 = 0x919ea5953a9a1ec9

