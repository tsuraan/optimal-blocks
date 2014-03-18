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

{-| Determine the hash of every 'len' sequence of bytes in the given
 'ByteString'. Because this uses BuzzHash, a rolling hash, this runs in @O(n)@
 time dependent on the length of 'bs', and independent of 'len'.

 This will generate @(length bs - len + 1)@ 64-bit hashes.
 -}
hashes :: Int         -- ^ How many bytes to put into each hash
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
        let prevH = Hash len $ unsafeLast v
            old8  = unsafeIndex bs (VU.length v - 1)
            new8  = unsafeIndex bs (len + VU.length v - 1)
        in currentVal $ roll prevH old8 new8

{-| A representation of a hash that allows rolling hashes to be easily
 calculated.
 -}
data Hash = Hash { windowLength :: !Int
                 , currentVal   :: !Word64
                 }
            deriving ( Show )

{-| Create a 'Hash' instance using an entire 'ByteString'. This doesn't have
 any sort of length argument to do partial 'ByteString's because 'ByteString'
 supports efficient slices on its own.
 -}
init :: ByteString -> Hash
init bs =
  let hash = BS.foldl upd 0 bs
  in Hash (length bs) hash
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
roll :: Hash -> Word8 -> Word8 -> Hash
roll hsh old new =
  let rem = (h old) `rotateL` (windowLength hsh)
      add = h new
      skh = (currentVal hsh) `rotateL` 1
  in hsh { currentVal = rem `xor` add `xor` skh }
{-# INLINE roll #-}

{-| Upgrade an 8-bit word to a 64-bit one that is very "different" from all
 the other possible 8-bit words. This library uses SipHash to do this.
 -}
h :: Word8 -> Word64
h = (hs !) . fromEnum

hs :: Vector Word64
hs = generate 256 fn
  where
  fn n = hashByteString 2 4 k0 k1 $ pack [toEnum n]
  k0 = 0x4a7330fae70f52e8
  k1 = 0x919ea5953a9a1ec9

