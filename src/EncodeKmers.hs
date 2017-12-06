{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleContexts, BangPatterns, PackageImports #-}

import qualified Data.Conduit as C
import           Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import           Data.Word
import           Data.Bits

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Control.Concurrent (setNumCapabilities)
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           Data.List (foldl')


import           Data.Conduit.Algorithms.Utils
import           Data.Conduit.Algorithms.Async

import Control.Monad.IO.Class
import System.IO
import Foreign.Storable
import Control.Monad

import Kmers
import Data.BioConduit

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
        hs <- liftIO $ sequence [openFile (base ++ "."++show i) WriteMode | i <- [0 :: Int ..15]]
        writeToSplits' hs
        forM_ hs (liftIO . hClose)
    where
        writeToSplits' hs = C.awaitForever $ \vs -> forM_ (zip hs vs) $ \(h, v) -> liftIO (writeV h v)
        writeV h v = VS.unsafeWith v $ \p ->
            hPutBuf h p (sizeOf (VS.head v) * VS.length v)

concatV :: V.Vector [VS.Vector Word32] -> [VS.Vector Word32]
concatV v = [(VS.concat . (map (!! i)) . V.toList $ v) | i <- [0..15]]

encodeKMERS' n fa = let ks = encodeKMERS fa
                        in VS.generate (VS.length ks * 2) (\ix -> if ix `mod` 2 == 0 then ks VS.! (ix `div` 2) else toEnum n)

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
            .| asyncMapC nthreads (concatV . V.map (splitTopV . uncurry encodeKMERS'))
            .| writeToSplits (ofileArg opts)
