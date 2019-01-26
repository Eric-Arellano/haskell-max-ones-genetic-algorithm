import Test.QuickCheck

main :: IO ()
main = do
    quickCheck prop_reverse

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs
