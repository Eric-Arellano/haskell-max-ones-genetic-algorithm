module Lib
    ( someFunc
    , mkIndividual
    , mkPopulation
    , bitStrToIndividual
    , individualToBitStr
    , fitness
    , mutate
    , crossover
    , compete
    ) where

import System.Random

someFunc :: IO ()
someFunc = putStrLn "Test"

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

mkIndividual :: RandomGen g => Int -> g -> Individual
mkIndividual nGenes gen = take nGenes $ randoms gen

mkPopulation :: RandomGen g => g -> Int -> Int -> Population
mkPopulation gen nInd nGenes = randomNTimes gen nInd (mkIndividual nGenes)

-- TODO: can this be implemened with fold? Abstracted into a general structure useful for generations?
-- Maybe generate a list of len nInd of RandomGen (use fold?), then map over that list with mkIndividual!
randomNTimes :: RandomGen g => g -> Int -> (g -> a) -> [a]
randomNTimes gen n f = recurse n gen []
  where
    recurse 0 _ result = result
    recurse n gen' result = recurse (n - 1) (nextGen gen') (f gen' : result)
    nextGen :: RandomGen g => g -> g
    nextGen gen' = snd $ next gen'

fitness :: Individual -> Int
fitness = length . filter (id)

mutate :: RandomGen g => g -> Float -> Individual -> Individual
mutate gen p = map (possiblyInvert) . zip randomList
  where
    possiblyInvert :: (Float, Bit) -> Bit 
    possiblyInvert (ran, x) = if ran > p then x else not x
    randomList = randomRs (0.0, 1.0) gen

crossover :: RandomGen g => g -> Float -> Individual -> Individual -> Individual
crossover gen p ind1 ind2 = ind1

compete :: Individual -> Individual -> Individual
compete i1 i2 = if fitness i1 >= fitness i2 then i1 else i2

