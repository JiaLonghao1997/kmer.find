{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleContexts, BangPatterns, PackageImports #-}

import qualified Data.ByteString as B
import qualified Data.Conduit as C
import           Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import           Data.Word
import           Data.Bits
import           Control.Monad
import           Control.Arrow

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import           Control.Concurrent (setNumCapabilities)
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           Data.List (foldl', sort, group)
import           Data.List.Extra (merge)

import           System.IO.MMap
import           Foreign.ForeignPtr
import           Foreign.Ptr

import           Data.Conduit.Algorithms.Utils
import           Data.Conduit.Algorithms.Async


import StorableConduit (writeWord32VS)

import Kmers (encodeKMERS)
import Data.BioConduit

type Kmer = Word32

data Index = Index
                { ixdIndices :: !(VS.Vector Word64)
                , idxData    :: !(VS.Vector Word32)
                }

findMatches :: Index -> VS.Vector Word32 -> [Word32]
findMatches ix = top 100 . map (VS.toList . extract ix) . VS.toList

extract :: Index -> Word32 -> VS.Vector Word32
extract (Index ix d) k = VS.slice (fromEnum $ ix VS.! fromEnum k) (fromEnum $ (ix VS.! fromEnum (k+1)) - (ix VS.! fromEnum k)) d

top :: Int -> [[Word32]] -> [Word32]
top n = map snd . take n . sort . asCounts . mergeMany

mergeMany [] = []
mergeMany [xs] = xs
mergeMany [x,y] = merge x y
mergeMany xs = merge (mergeMany $ every2 xs) (mergeMany $ every2 (tail xs))

every2 [] = []
every2 [x] = [x]
every2 (x:_:xs) = x:every2 xs

asCounts :: Eq a => [a] -> [(Int, a)]
asCounts = map (length &&& head) . group

data CmdArgs = CmdArgs
                    { ifileArg :: FilePath
                    , ofileArg :: FilePath
                    , index1Arg :: FilePath
                    , index2Arg :: FilePath
                    , verboseArg :: Bool
                    , nJobsArg :: Int
                    } deriving (Show)


parseArgs :: [String] -> CmdArgs
parseArgs argv = foldl' (flip ($)) (CmdArgs "" "" "" "" False 1) flags
    where
        flags :: [CmdArgs -> CmdArgs]
        (flags, _args, _extraOpts) = getOpt Permute options argv
        options =
            [ Option ['o'] ["output"] (ReqArg (\f c -> c { ofileArg = f }) "FILE") "Output file"
            , Option ['i'] ["input"] (ReqArg (\f c -> c { ifileArg = f }) "FILE") "Input file to check"
            , Option ['1'] ["index1"] (ReqArg (\f c -> c { index1Arg = f }) "FILE") "Index (file 1)"
            , Option ['2'] ["index2"] (ReqArg (\f c -> c { index2Arg = f }) "FILE") "Index (file 2)"
            , Option ['v'] ["verbose"] (NoArg (\c -> c {verboseArg = True }))  "Verbose"
            , Option ['t'] ["threads"] (ReqArg (\n c -> c { nJobsArg = read n }) "N") "Nr Threads"
            ]


main :: IO ()
main = do
    opts <- parseArgs <$> getArgs
    print opts
    let nthreads = nJobsArg opts
    setNumCapabilities nthreads
    qs <- C.runConduitRes $
        CB.sourceFile (ifileArg opts)
            .| faConduit
            .| CC.sinkList
    mmapWithFilePtr (index1Arg opts) ReadOnly Nothing $ \(p1, s1) -> do
        mmapWithFilePtr (index2Arg opts) ReadOnly Nothing $ \(p2, s2) -> do
            p1' <- newForeignPtr_ $ castPtr p1
            p2' <- newForeignPtr_ $ castPtr p2
            let ix = Index (VS.unsafeFromForeignPtr0 p1' (s1 `div` 8)) (VS.unsafeFromForeignPtr0 p2' (s2 `div` 4))
            forM_ qs (putStrLn . show . findMatches ix . encodeKMERS 0)
