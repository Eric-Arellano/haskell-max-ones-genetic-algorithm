module Main where

import qualified Lib
import qualified Config

import qualified System.Random as Random

main :: IO ()
main = do
    config <- Config.readConfig "resources/config.cfg"
    let pop = Lib.mkPopulation (Random.mkStdGen $ Config.seed config) (Config.numIndividuals config) (Config.numGenes config)
    putStrLn $ show $ map Lib.individualToBitStr pop
