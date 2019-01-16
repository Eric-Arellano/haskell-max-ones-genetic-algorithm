module Main where

import qualified Lib
import qualified Config

main :: IO ()
main = do
    config <- Config.readConfig "resources/config.cfg"
    putStrLn $ show config
