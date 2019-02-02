{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( mkIndividual
    , mkPopulation
    , bitStrToIndividual
    , individualToBitStr
    , fitness
    , avgFitness
    , maxFitness
    , mutate
    , crossover
    , compete
    ) where

import System.Random
import qualified Data.List
import qualified Data.Ord

-- --------------------------------------------------------
-- Data model
-- --------------------------------------------------------

type BitStr = [Char]
type Bit = Bool
type Individual = [Bit]
type Population = [Individual]

bitStrToIndividual :: BitStr -> Individual
bitStrToIndividual = map ( == '1') 

individualToBitStr :: Individual -> BitStr
individualToBitStr = map (toStr)
  where
    toStr True = '1'
    toStr False = '0'

-- --------------------------------------------------------
-- Initialize data
-- --------------------------------------------------------

mkIndividual :: forall g. RandomGen g => Int -> g -> Individual
mkIndividual nGenes gen = take nGenes $ randoms gen

mkPopulation :: forall g. RandomGen g => g -> Int -> Int -> Population
mkPopulation gen nInd nGenes = randomNTimes gen nInd (mkIndividual nGenes)

randomNTimes :: forall a g. RandomGen g => g -> Int -> (g -> a) -> [a]
randomNTimes gen n f = map f genList
  where
    genList :: [g]
    genList = genRecurse n gen []
    genRecurse :: Int -> g -> [g] -> [g]
    genRecurse 0 _ result = result
    genRecurse n' gen' result = genRecurse (n' - 1) (nextGen gen') (gen' : result)
    nextGen :: g -> g
    nextGen gen' = snd $ next gen'

-- --------------------------------------------------------
-- Fitness
-- --------------------------------------------------------

fitness :: Individual -> Int
fitness = length . filter (id)

avgFitness :: Population -> Float
avgFitness pop = sum popFit / fromIntegral (length pop)
  where
    popFit = map (fromIntegral . fitness) pop

maxFitness :: Population -> Int
maxFitness = maximum . map fitness

-- --------------------------------------------------------
-- Mutation
-- --------------------------------------------------------

mutate :: forall g. RandomGen g => g -> Float -> Individual -> Individual
mutate gen p = map (possiblyInvert) . zip randomList
  where
    possiblyInvert :: (Float, Bit) -> Bit 
    possiblyInvert (ran, x) = if ran < p then not x else x
    randomList = randomRs (0.0, 1.0) gen

-- --------------------------------------------------------
-- Crossover
-- --------------------------------------------------------
crossover :: forall g. RandomGen g => g -> Float -> Individual -> Individual -> (Individual, Individual)
crossover gen p ind1 ind2 = if ran < p then (crossed1, crossed2) else (ind1, ind2)
  where
    crossed1 = (take crossPoint ind1) ++ (drop crossPoint ind2)
    crossed2 = (take crossPoint ind2) ++ (drop crossPoint ind1)
    (ran, gen') = randomR (0.0, 1.0) gen
    (crossPoint, _) = randomR (1, length ind1 - 1) gen'

-- --------------------------------------------------------
-- Selection
-- --------------------------------------------------------

-- This is used in conjunction with tournament selection. It can be passed a list
-- of individuals (size k, or the tournament size), and will return one winner. The
-- winner is simply calculated as the most fit. Note an alternative implementation is to
-- sort by fitness then give each the probability w_n = p*((1-p)^(n-1)) to win,
-- with n startinag at 0.
compete :: [Individual] -> Individual
compete = Data.List.maximumBy (Data.Ord.comparing fitness) 

-- Tournament selection. While the new population is smaller than the original population,
-- repeatedly choose k individuals from the original population; it's okay to repeat.
-- For those k invidivudals, call the compete function to get the one winner from those k
-- individuals. Add that winner to the new population, and repeat until the populations are the same size.
tournament :: forall g. RandomGen g => g -> Int -> Population -> Population
tournament gen k pop = ???
  where
    recurse :: g -> Population -> Population
    recurse gen' newPop
      | length(newPop) == length(pop) = newPop
      | otherwise = recurse (nextGen gen') (findWinner : newPop)
    findWinner :: Individual
    findWinner = ???
    nextGen :: g -> g
    nextGen gen' = snd $ next gen'

