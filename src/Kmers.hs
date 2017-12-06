{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleContexts, BangPatterns, PackageImports #-}
module Kmers
    ( encodeKMERS
    ) where

import qualified Data.ByteString as B
import           Data.Word
import           Data.Bits

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import Data.BioConduit

alphabetIndex :: VU.Vector Word32
alphabetIndex = VU.fromList
                    [ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
                    , 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
                    , 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  7,  0,  0, 13, 12,  1,  5, 11,  4,  0
                    ,15,  3,  3, 10,  0,  6, 12, 14,  9,  8,  0,  4,  2,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
                    , 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
                    , 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
                    , 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
                    , 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
                    , 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
                    , 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
                    , 0,  0,  0,  0,  0,  0]

encodeKMERS :: Fasta -> VS.Vector Word32
encodeKMERS (Fasta _ fa) = VS.fromList . drop 7 $ scanl k1 0 (B.unpack fa)
    where
        k1 :: Word32 -> Word8 -> Word32
        k1 k b = let b' = alphabetIndex VU.! fromEnum b
                        in ((k .&. 0xffffff) `shiftL` 4) .|. b'
