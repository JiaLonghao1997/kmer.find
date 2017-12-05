{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleContexts, BangPatterns, PackageImports #-}

import qualified Data.Conduit as C
import           Data.Conduit ((.|))
import qualified Data.Conduit.Binary as CB
import           Data.Binary
import Control.Monad.IO.Class

import qualified Data.Vector.Storable as VS
import           Control.Concurrent (setNumCapabilities)
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           Control.Monad
import           Data.List (foldl')
import System.IO.SafeWrite (withOutputFile)
import StorableConduit (readWord32VS)
import System.IO
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.List
import Control.Monad
import Control.Arrow

import           Data.Conduit.Algorithms.Async


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


writeOut :: (Storable a, MonadIO m) => Handle -> Handle -> C.Sink ([(Word32, Int)], VS.Vector a) m ()
writeOut hi hd = do
        write64 hi 0
        writeOut' 0 (0 :: Word64)
    where
        writeOut' k pos = C.await >>= \case
            Just (ix, d) -> do
                liftIO $ writeV hd d
                (k', pos') <- proc k pos ix
                writeOut' k' pos'

            Nothing -> do
                liftIO $ putStrLn $ "Last k: " ++ show k
                void $ proc k pos [(finalK, 0)]
        proc :: MonadIO m => Word32 -> Word64 -> [(Word32, Int)] -> m (Word32, Word64)
        proc !k !pos [] = return (k, pos)
        proc !k !pos t@((!k',!c):ks) = case compare k k' of
            GT -> error ("SHOULD NEVER HAVE HAPPENED (" ++ show k ++ "< " ++ show k' ++")")
            EQ -> proc k (pos + toEnum c) ks
            LT -> do
                if k' > finalK
                    then error $ "Saw "++show k' ++ " > " ++show finalK
                    else do
                        write64 hi pos
                        proc (k+1) pos t

        writeV h v = VS.unsafeWith v $ \p ->
            hPutBuf h p (sizeOf (VS.head v) * VS.length v)
        write64 :: MonadIO m => Handle -> Word64 -> m ()
        write64 h val = liftIO . alloca $ \p -> do
                poke p val
                hPutBuf h p (sizeOf val)
        finalK :: Word32
        finalK = 2^(29 :: Word32)

splitVector :: VS.Vector Word32 -> ([(Word32, Int)], VS.Vector Word32)
splitVector v = (map (head &&& length) . group . every2 . VS.toList $ v, VS.generate (n `div` 2) (\ix -> v VS.! (2 * ix + 1)))
    where
        n = VS.length v
        every2 :: [Word32] -> [Word32]
        every2 [] = []
        every2 [x] = [x]
        every2 (x:_:xs) = x:every2 xs

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
                    .| readWord32VS 4096
                    .| asyncMapC nthreads splitVector
                    .| writeOut hi hd
