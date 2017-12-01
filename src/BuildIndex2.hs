{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleContexts, BangPatterns, PackageImports #-}

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import           Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import           Data.Binary.Put
import           Data.Binary
import           Data.Word
import           Data.Bits
import Control.Monad.IO.Class

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import           Control.Concurrent (setNumCapabilities)
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           Control.Monad
import           Data.List (foldl')
import System.IO.SafeWrite (withOutputFile)
import StorableConduit (readWord32VS)


import           Data.Conduit.Algorithms.Utils
import           Data.Conduit.Algorithms.Async



import Data.BioConduit

data CmdArgs = CmdArgs
    { ifileArg :: FilePath
    , ofileArg1 :: FilePath
    , ofileArg2 :: FilePath
    , verboseArg :: Bool
    , nJobsArg :: Int
    } deriving (Show)


parseArgs :: [String] -> CmdArgs
parseArgs argv = foldl' (flip ($)) (CmdArgs "" "" "" False 1) flags
    where
        flags :: [CmdArgs -> CmdArgs]
        (flags, _args, _extraOpts) = getOpt Permute options argv
        options =
            [ Option ['o'] ["output"] (ReqArg (\f c -> c { ofileArg1 = f }) "FILE") "Output file"
            , Option ['p'] ["output2"] (ReqArg (\f c -> c { ofileArg2 = f }) "FILE") "Output file"
            , Option ['i'] ["input"] (ReqArg (\f c -> c { ifileArg = f }) "FILE") "Input file to check"
            , Option ['v'] ["verbose"] (NoArg (\c -> c {verboseArg = True }))  "Verbose"
            , Option ['t'] ["threads"] (ReqArg (\n c -> c { nJobsArg = read n }) "N") "Nr Threads"
            ]


writeOut hi hd = awaitJust $ \v -> do
        let k = v VS.! 0
            ix = v VS.! 1
        writeIxFromTo 0 k 0
        write32 hd ix
        writeOut' k 1
    where
        write32 h val = liftIO $ do
            BL.hPut h (runPut $ putWord32le val)
        writeIxFromTo s e val = forM_ [s..e] $ \v -> write32 hi val
        finalK :: Word32
        finalK = 2^28
        writeOut' !k !n = C.await >>= \case
            Nothing -> writeIxFromTo (k+1) finalK (n+1)
            Just v -> do
                let k' = v VS.! 0
                    ix = v VS.! 1
                write32 hd ix
                when (k /= k') $
                    writeIxFromTo k k' n
                writeOut' k' (n+1)

main :: IO ()
main = do
    opts <- parseArgs <$> getArgs
    print opts
    let nthreads = nJobsArg opts
    setNumCapabilities nthreads
    withOutputFile (ofileArg1 opts) $ \hi ->
        withOutputFile (ofileArg2 opts) $ \hd ->
            C.runConduitRes $
                CB.sourceFile (ifileArg opts)
                    .| readWord32VS 2
                    .| writeOut hi hd
