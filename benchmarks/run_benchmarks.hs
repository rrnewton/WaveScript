{-# LANGUAGE NamedFieldPuns #-}

-- | HSBencher script to run all the benchmarks.
module Main where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import System.Directory
import System.Exit
import System.Environment (getEnv)
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)

import HSBencher (defaultMainModifyConfig)
import HSBencher.Types
import HSBencher.Internal.Utils (runLogged)

import HSBencher.Backend.Fusion  (defaultFusionPlugin)
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMainModifyConfig $ \conf -> conf
  { benchlist = benches
  , buildMethods = [ wsc2 ]
  , plugIns = [ SomePlugin defaultFusionPlugin ]
  }

benches :: [Benchmark DefaultParamMeaning]
benches = [ mkBenchmark (bench "pipeline/num_simple_pipeline.ws") tuples spec 
          , mkBenchmark (bench "splitjoin/num_splitjoin.ws") tuples spec
          ]
  where bench = ("microbench/" ++)
        tuples = ["-n", "250000"]
        -- It seems that I can't set spec to `Or []` or else benches that use it
        -- will never be run. So we'll set this garbage instead. Ugh.
        --spec = Set NoMeaning $ RuntimeEnv "_IGNORE_" ""
        spec = Variant (numops++"ops") `Set` RuntimeEnv "NUMOPS" numops
        numops = unsafePerformIO $ getEnv "NUMOPS"

wsc2 :: BuildMethod
wsc2 = BuildMethod
  { methodName = "wsc2"
  , canBuild = WithExtension ".ws"
  , concurrentBuild = False
  , setThreads = Nothing
  , clean = \_ _ _ -> return ()
  , compile = \_ _ _ target -> do
      let (dir,file) = splitFileName target
      lift $ setCurrentDirectory dir
      runSuccessful " [wsc2] " $ "wsc2 -noprint " ++ file
      lift $ setCurrentDirectory "../.."
      return . StandAloneBinary $ dir ++ "query.exe"
  }

runSuccessful :: String -> String -> BenchM [B.ByteString]
runSuccessful tag cmd = do
  (res,lines) <- runLogged tag cmd
  case res of
    ExitError code  -> error $ "expected this command to succeed! But it exited with code "++show code++ ":\n  "++ cmd
    RunTimeOut {}   -> error "Methods.hs/runSuccessful - internal error!"
    RunCompleted {} -> return lines
