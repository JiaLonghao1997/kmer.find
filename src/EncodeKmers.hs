{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleContexts, BangPatterns, PackageImports #-}

import qualified Data.ByteString as B
import qualified Data.Conduit as C
import           Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import           Data.Word
import           Data.Bits

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import           Control.Concurrent (setNumCapabilities)
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           Data.List (foldl')


import           Data.Conduit.Algorithms.Utils
import           Data.Conduit.Algorithms.Async

import Control.Monad.IO.Class
import System.IO.SafeWrite (withOutputFile)
import StorableConduit (readWord32VS)
import System.IO
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.List
import Control.Arrow
import Control.Monad

import StorableConduit (writeWord32VS)


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

encodeKMERS :: Int -> Fasta -> VS.Vector Word32
encodeKMERS n (Fasta _ fa) = VS.fromList . concatMap (\k -> [k, toEnum n]) . drop 7 $ scanl k1 0 (B.unpack fa)
    where
        k1 :: Word32 -> Word8 -> Word32
        k1 k b = let b' = alphabetIndex VU.! fromEnum b
                        in ((k .&. 0xffffff) `shiftL` 4) .|. b'

splitTopV :: VS.Vector Word32 -> [VS.Vector Word32]
splitTopV v = [get k | k <- [0..15]]
    where
        nelem = VS.length v `div` 2
        ixs = VS.fromList [v VS.! (i*2) `shiftR` 24 | i <- [0..nelem - 1]]
        get k = VS.ifilter (\ix _ -> ixs VS.! (ix `div` 2) == k) v

data CmdArgs = CmdArgs
    { ifileArg :: FilePath
    , ofileArg :: FilePath
    , verboseArg :: Bool
    , nJobsArg :: Int
    } deriving (Show)


parseArgs :: [String] -> CmdArgs
parseArgs argv = foldl' (flip ($)) (CmdArgs "" "" False 1) flags
    where
        flags :: [CmdArgs -> CmdArgs]
        (flags, _args, _extraOpts) = getOpt Permute options argv
        options =
            [ Option ['o'] ["output"] (ReqArg (\f c -> c { ofileArg = f }) "FILE") "Output file"
            , Option ['i'] ["input"] (ReqArg (\f c -> c { ifileArg = f }) "FILE") "Input file to check"
            , Option ['v'] ["verbose"] (NoArg (\c -> c {verboseArg = True }))  "Verbose"
            , Option ['t'] ["threads"] (ReqArg (\n c -> c { nJobsArg = read n }) "N") "Nr Threads"
            ]


writeToSplits base = do
        hs <- liftIO $ sequence [openFile (base ++ "."++show i) WriteMode | i <- [0..15]]
        writeToSplits' hs
        forM_ hs (liftIO . hClose)
    where
        writeToSplits' hs = C.awaitForever $ \vs -> forM_ (zip hs vs) $ \(h, v) -> liftIO (writeV h v)
        writeV h v = VS.unsafeWith v $ \p ->
            hPutBuf h p (sizeOf (VS.head v) * VS.length v)

concatV :: V.Vector [VS.Vector Word32] -> [VS.Vector Word32]
concatV v = [(VS.concat . (map (!! i)) . V.toList $ v) | i <- [0..15]]

main :: IO ()
main = do
    opts <- parseArgs <$> getArgs
    print opts
    let nthreads = nJobsArg opts
    setNumCapabilities nthreads
    C.runConduitRes $
        CB.sourceFile (ifileArg opts)
            .| faConduit
            .| enumerateC
            .| CC.conduitVector 8192
            .| asyncMapC nthreads (concatV . V.map (splitTopV . uncurry encodeKMERS))
            .| writeToSplits (ofileArg opts)
