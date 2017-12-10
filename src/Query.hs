{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleContexts, BangPatterns, PackageImports #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit as C
import           Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import           Data.Word
import           Control.Monad

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Control.Concurrent (setNumCapabilities)
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           Data.List (foldl', sortBy)
import           Data.List.Extra (merge)

import           System.IO.MMap
import           Foreign.ForeignPtr
import           Foreign.Ptr

import           Data.Conduit.Algorithms.Async
import           Control.Monad.IO.Class


import Kmers (encodeKMERS)
import Data.BioConduit

data Index = Index
                { ixdIndices :: !(VS.Vector Word64)
                , idxData    :: !(VS.Vector Word32)
                }

findMatches :: Index -> VS.Vector Word32 -> [Word32]
findMatches ix = top 100 . map (uniq . VS.toList . extract ix) . VS.toList

extract :: Index -> Word32 -> VS.Vector Word32
extract (Index ix d) k = VS.slice (fromEnum $ ix VS.! fromEnum k) (fromEnum $ (ix VS.! fromEnum (k+1)) - (ix VS.! fromEnum k)) d

top :: Int -> [[Word32]] -> [Word32]
top n = map snd . topNBy n (\a b -> compare b a) . mergeMany . map (map $ \v -> (1 :: Int, v))

topNBy :: Int -> (a -> a -> Ordering) -> [a] -> [a]
topNBy n f xs = let
                    -- 1. take first N
                    -- 2. sort them (so that smallest is first)
                    -- 3. insert rest of elements, preserving order
                    (top, rest) = splitAt n xs
                    insert [] r = r
                    insert (x:xs) r = insert xs (insert1 x r)
                    insert1 x cur@(r:rs)
                        | f x r == GT = cur
                        | otherwise = put1 x rs
                    put1 x [] = [x]
                    put1 x (r:rs)
                        | f x r == GT = x:r:rs
                        | otherwise = r:put1 x rs
                in reverse $ insert rest (sortBy (\a b -> f b a) top)

uniq [] = []
uniq [x] = [x]
uniq (x:y:xs)
    | x == y = uniq (y:xs)
    | otherwise = x:uniq (y:xs)

mergeMany [] = []
mergeMany [xs] = xs
mergeMany [x,y] = merge2 x y
mergeMany xs = merge2 (mergeMany $ every2 xs) (mergeMany $ every2 (tail xs))

merge2 xs [] = xs
merge2 [] xs = xs
merge2 xs@(h0@(c0,v0):xss) ys@(h1@(c1,v1):yss) = case compare v0 v1 of
    EQ -> (c0 + c1, v0):merge2 xss yss
    LT -> h0:merge2 xss ys
    GT -> h1:merge2 xs yss

every2 [] = []
every2 [x] = [x]
every2 (x:_:xs) = x:every2 xs

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


printMatch (Fasta fah _) matches = do
    putStrLn $ ">" ++ B8.unpack fah
    forM_ matches (putStrLn . show)

main :: IO ()
main = do
    opts <- parseArgs <$> getArgs
    print opts
    let nthreads = nJobsArg opts
    setNumCapabilities nthreads
    mmapWithFilePtr (index1Arg opts) ReadOnly Nothing $ \(p1, s1) -> do
        mmapWithFilePtr (index2Arg opts) ReadOnly Nothing $ \(p2, s2) -> do
            p1' <- newForeignPtr_ $ castPtr p1
            p2' <- newForeignPtr_ $ castPtr p2
            let ix = Index (VS.unsafeFromForeignPtr0 p1' (s1 `div` 8)) (VS.unsafeFromForeignPtr0 p2' (s2 `div` 4))
            C.runConduitRes $
                CB.sourceFile (ifileArg opts)
                    .| faConduit
                    .| asyncMapC (2*nthreads) (\q -> (q, findMatches ix $ encodeKMERS q))
                    .| CC.mapM_ (liftIO . uncurry printMatch)
