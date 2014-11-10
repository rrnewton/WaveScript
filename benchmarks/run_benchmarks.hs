{-# LANGUAGE NamedFieldPuns #-}

-- | HSBencher script to run all the benchmarks.
module Main where

import qualified Data.ByteString.Char8 as B
import System.Exit
import System.FilePath
import System.Directory
import Control.Monad.Trans

import HSBencher (defaultMainModifyConfig)
import HSBencher.Types
import HSBencher.Internal.Utils (runLogged)

--import HSBencher.Backend.Fusion  (defaultFusionPlugin)
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMainModifyConfig $ \conf -> conf
  { benchlist = benches
  , buildMethods = [ wsc2 ]
  , plugIns = [ ]
  }

benches :: [Benchmark DefaultParamMeaning]
benches = [ mkBenchmark (bench "pipeline/num_pipe.ws") tuples compileSpec
            | tuples <- runtimeSpec ]
  where bench = ("microbench/" ++)
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
    let (dir,file) = splitFileName target
    lift $ putStrLn $ " [wsc2] Compiling file "++file++" in dir "++dir
    lift $ setCurrentDirectory dir 
    runSuccessful " [wsc2] " $ "wsc2 " ++ file
    return $ StandAloneBinary "./query.exe"
  }

runSuccessful :: String -> String -> BenchM [B.ByteString]
runSuccessful tag cmd = do
  (res,lines) <- runLogged tag cmd
  case res of
    ExitError code  -> error $ "expected this command to succeed! But it exited with code "++show code++ ":\n  "++ cmd
    RunTimeOut {}   -> error "Methods.hs/runSuccessful - internal error!"
    RunCompleted {} -> return lines
