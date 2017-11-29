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
encodeKMERS n (Fasta _ fa) = VS.fromList . concat $ [[k, toEnum n] | k <- kmers]
    where
        kmers = map encodeKmer $ slidingWindow 7 fa
        slidingWindow :: Int -> B.ByteString -> [B.ByteString]
        slidingWindow ws b
            | ws > B.length b = []
            | otherwise = B.take ws b:slidingWindow ws (B.tail b)
        encodeKmer :: B.ByteString -> Word32
        encodeKmer = encodeKmer' 0
        encodeKmer' acc b = case B.uncons b of
            Nothing -> acc
            Just (h, rest) -> encodeKmer' (acc `shiftL` 4 .|. (alphabetIndex VU.! fromEnum h)) rest

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
            .| asyncMapC nthreads (V.map (uncurry encodeKMERS))
            .| CC.concat
            .| writeWord32VS
            .| CB.sinkFileCautious (ofileArg opts)
