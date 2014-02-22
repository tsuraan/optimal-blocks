module Algorithm.OptimalBlocks
( chop
, Block(..)
) where

import Data.ByteString ( ByteString, length, splitAt)
import Prelude hiding ( length, splitAt, max )

data Block = BoundaryFound ByteString ByteString
           | NoBoundaryFound ByteString
           deriving ( Show )

chop :: ByteString -> Block
chop = chop' 128 (2 * mb)
  where
  mb = 1024*1024

chop' :: Int        -- ^ Window size for rolling hash
      -> Int        -- ^ Max allowed block size
      -> ByteString -- ^ ByteString to chop
      -> Block
chop' _ max bs
  | length bs < max = NoBoundaryFound bs
  | otherwise = let (block, rest) = splitAt max bs in BoundaryFound block rest
