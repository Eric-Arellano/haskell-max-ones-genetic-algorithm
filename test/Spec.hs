{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Random (mkStdGen)
import Test.QuickCheck (quickCheck)

import qualified Lib

main :: IO ()
main = do
    quickCheck $ prop_sameLength (Lib.mutate (mkStdGen 1) 0.4)

prop_sameLength :: forall a. ([a] -> [a]) -> [a] -> Bool
prop_sameLength f xs = length (f xs) == length xs 
