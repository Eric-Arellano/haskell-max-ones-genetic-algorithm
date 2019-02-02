module Config
    ( ConfigInfo(..)
    , readConfig
    ) where

import qualified Data.ConfigFile as CF
import Control.Monad.Except

-- See http://hackage.haskell.org/package/ConfigFile-1.1.4/docs/Data-ConfigFile.html for docs
-- and http://tuttlem.github.io/2013/07/04/configfile-basics-in-haskell.html for guide

data ConfigInfo = ConfigInfo { seed :: Int
                             , mutation :: Float
                             , crossover :: Float
                             , numGenes :: Int
                             , numIndividuals :: Int
                             , numGenerations :: Int
                             , tournamentSize :: Int
                             } deriving (Show)

readConfig :: String -> IO ConfigInfo
readConfig f = do
    rv <- runExceptT $ do
        -- open config file
        cp <- join $ liftIO $ CF.readfile CF.emptyCP f

        -- read attributes
        seedV <- CF.get cp "DEFAULT" "seed"
        mutationV <- CF.get cp "DEFAULT" "mutation"
        crossoverV <- CF.get cp "DEFAULT" "crossover"
        genesV <- CF.get cp "DEFAULT" "num_genes"
        individualsV <- CF.get cp "DEFAULT" "num_individuals"
        generationsV <- CF.get cp "DEFAULT" "num_generations"
        tournamentSizeV <- CF.get cp "DEFAULT" "tournament_size"

        -- build config value
        return (ConfigInfo { seed = seedV
                           , mutation = mutationV
                           , crossover = crossoverV
                           , numGenes = genesV
                           , numIndividuals = individualsV
                           , numGenerations = generationsV
                           , tournamentSize = tournamentSizeV
                           })
    -- either error out or return value
    either (error . snd) (return) rv

