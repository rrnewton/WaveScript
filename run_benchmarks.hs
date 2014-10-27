{-# LANGUAGE NamedFieldPuns #-}

-- | HSBencher script to run all the benchmarks.
module Main where

import qualified Data.ByteString.Char8 as B
import System.Exit

import HSBencher (defaultMainModifyConfig)
import HSBencher.Types
import HSBencher.Internal.Utils (runLogged)

--import HSBencher.Backend.Fusion  (defaultFusionPlugin)
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMainModifyConfig $ \conf -> conf
  { benchlist = benches
  , buildMethods = [ wsc2 ]
  , plugIns = []
  }

benches :: [Benchmark DefaultParamMeaning]
benches = [ mkBenchmark (bench "pipline/num_pipe.ws") tuples compileSpec
            | tuples <- runtimeSpec ]
  where bench = ("benchmarks/microbenchmarks/" ++)
        runtimeSpec = [ ["-n", show n] | n <- [40,80 .. 400] ]
        compileSpec = Or [ Set NoMeaning . RuntimeEnv "NUMOPS" $ show ops
                           | ops <- [16,32 .. 400] ]

wsc2 :: BuildMethod
wsc2 = BuildMethod
  { methodName = "wsc2"
  , canBuild = WithExtension ".ws"
  , concurrentBuild = False
  , setThreads = Nothing
  , clean = \_ _ _ -> return ()
  , compile = \_ _ _ target -> do
      runSuccessful " [wsc2] " $ "wsc2 " ++ target
      return $ StandAloneBinary "./query.exe"
  }

runSuccessful :: String -> String -> BenchM [B.ByteString]
runSuccessful tag cmd = do
  (res,lines) <- runLogged tag cmd
  case res of
    ExitError code  -> error $ "expected this command to succeed! But it exited with code "++show code++ ":\n  "++ cmd
    RunTimeOut {}   -> error "Methods.hs/runSuccessful - internal error!"
    RunCompleted {} -> return lines
