{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleContexts, BangPatterns, PackageImports #-}
import           Data.Word
import           Algorithms.SortMain (sortMainStorable)
import           Foreign
import           Control.DeepSeq


data Word32Pair = Word32Pair {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32
    deriving (Eq, Show)

instance NFData Word32Pair where
    rnf !_ = ()

instance Ord Word32Pair where
    compare (Word32Pair a0 b0) (Word32Pair a1 b1)
        | a0 /= a1 = compare b0 b1
        | otherwise = compare a0 a1

instance Storable Word32Pair where
    sizeOf _ = 2 * sizeOf (undefined :: Word32)
    alignment _ = alignment (undefined :: Word32)

    peek p = let p' = castPtr p in
            Word32Pair
                <$> peekElemOff p' 0
                <*> peekElemOff p' 1
    poke p (Word32Pair e0 e1) = do
        let p' = castPtr p
        pokeElemOff p' 0 e0
        pokeElemOff p' 1 e1

main :: IO ()
main = sortMainStorable (undefined :: Word32Pair) (4000*1000*1000)
