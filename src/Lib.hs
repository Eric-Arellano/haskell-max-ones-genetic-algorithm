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

fitness :: Individual -> Int
fitness = length . filter (id)

avgFitness :: Population -> Float
avgFitness pop = sum popFit / fromIntegral (length pop)
  where
    popFit = map (fromIntegral . fitness) pop

maxFitness :: Population -> Int
maxFitness = maximum . map fitness

mutate :: RandomGen g => g -> Float -> Individual -> Individual
mutate gen p = map (possiblyInvert) . zip randomList
  where
    possiblyInvert :: (Float, Bit) -> Bit 
    possiblyInvert (ran, x) = if ran > p then x else not x
    randomList = randomRs (0.0, 1.0) gen

crossover :: RandomGen g => g -> Float -> Individual -> Individual -> (Individual, Individual)
crossover gen p ind1 ind2 = if ran > p then (crossed1, crossed2) else (ind1, ind2)
  where
    crossed1 = (take crossPoint ind1) ++ (drop crossPoint ind2)
    crossed2 = (take crossPoint ind2) ++ (drop crossPoint ind1)
    (ran, gen') = randomR (0.0, 1.0) gen
    (crossPoint, _) = randomR (1, length ind1 - 1) gen'

compete :: Individual -> Individual -> Individual
compete i1 i2 = if fitness i1 >= fitness i2 then i1 else i2

