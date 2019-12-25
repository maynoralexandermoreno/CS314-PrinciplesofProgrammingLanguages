module CheckPA1 where

import System.Timeout
import Control.Exception (catch, evaluate, SomeException)
import Project1 (triples, hailstone, Point(..), centroid, Tree(..), mirror, strict, near_balanced)

-- To use this test script, place it in the same directory as your Project1.hs
-- file, load it into GHCi, and enter main at the prompt.
--
-- Passing the 20 tests provided here is sufficient to get 50% credit for this
-- project. To get the remaining points, your functions must be correct for all
-- inputs (excluding [] for centroid and non-positive integers for hailstone).
--
-- During development, you may comment out tests for functions you have not
-- yet completed. You may add additional tests of your own, but these tests
-- will not contribute directly towards your score.

tests =
    [ test_triples [1,2,3,4,5,6] 2
    , test_triples [3,33,333] 3
    , test_triples [0,1,4] 1
    , test_triples [] 0

    , test_hailstone 4 [4,2,1]
    , test_hailstone 6 [6,3,10,5,16,8,4,2,1]
    , test_hailstone 7 [7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]
    , test_hailstone 1 [1]

    , test_centroid [Pt 1 1, Pt 2 2] (Pt 1.5 1.5)
    , test_centroid [Pt (-1.5) 0, Pt 3 2, Pt 0 1] (Pt 0.5 1.0)
    , test_centroid [Pt 8 6, Pt 5.5 4, Pt 100 0, Pt (-29) 17] (Pt 21.125 6.75)
    , test_centroid [Pt 10 10] (Pt 10 10)

    , test_mirror (Bin Tip 1 (Bin Tip 2 Tip)) (Bin (Bin Tip 2 Tip) 1 Tip)
    , test_mirror Tip (Tip :: Tree ())
    , test_mirror (Bin Tip 1 Tip) (Bin Tip 1 Tip)
    , test_mirror (Bin (Bin Tip 'a' Tip) 'b' (Bin Tip 'c' Tip))
                  (Bin (Bin Tip 'c' Tip) 'b' (Bin Tip 'a' Tip))

    , test_strict (Bin Tip 1 Tip) True
    , test_strict (Bin Tip 1 (Bin Tip 2 Tip)) False
    , test_strict (Tip :: Tree ()) True
    , test_strict (Bin (Bin (Bin Tip 'a' Tip) 'b' (Bin Tip 'c' Tip)) 'd' (Bin Tip 'e' Tip)) True

    , test_near_balanced (Bin Tip 1 Tip) True
    , test_near_balanced (Bin (Bin Tip 1 Tip) 1 Tip) True
    , test_near_balanced
        (Bin (Bin Tip 2 Tip) 1 (Bin (Bin Tip 3 Tip) 2 Tip)) True
    , test_near_balanced
        (Bin (Bin Tip 2 Tip) 1 (Bin (Bin Tip 3 (Bin Tip 4 Tip)) 2 Tip)) False
    ]



timelimit = 1000000

testBy :: (Show a) => (a -> a -> Bool) -> String -> a -> a -> IO Bool
testBy eq name got want = catch body handle
    where
    body = do
        -- putStrLn $ "testing " ++ name
        ok <- timeout timelimit (evaluate (eq got want))
        case ok of
            Just True -> return True
            Just False -> fail "INCORRECT" (show got)
            Nothing    -> fail "TIMEOUT" ""

    handle :: SomeException -> IO Bool
    handle e = fail "ERROR" (show e)

    fail msg txt = do
        putStrLn $ msg ++ ": " ++ name
        putStrLn $ "       wanted: " ++ show want
        putStrLn $ "       got:    " ++ take 200 txt
        putStrLn ""
        return False


test :: (Eq a, Show a) => String -> a -> a -> IO Bool
test = testBy (==)


test_f1 :: (Eq b, Show a, Show b) => String -> (a -> b) -> a -> b -> IO Bool
test_f1 name f x r = test fx (f x) r
    where fx = (showString name . showChar ' ' . showsPrec 11 x) ""

test_triples       = test_f1 "triples" triples
test_hailstone     = test_f1 "hailstone" hailstone
test_centroid      = test_f1 "centroid" centroid

test_mirror :: (Eq a, Show a) => Tree a -> Tree a -> IO Bool
test_mirror        = test_f1 "mirror" mirror

test_strict :: (Eq a, Show a) => Tree a -> Bool -> IO Bool
test_strict        = test_f1 "strict" strict

test_near_balanced :: (Eq a, Show a) => Tree a -> Bool -> IO Bool
test_near_balanced = test_f1 "near_balanced" near_balanced


runTests score [] = return score
runTests score (t:ts) = do
    b <- t
    runTests (if b then score + 1 else score) ts

main = do
    score <- runTests 0 tests
    putStrLn $ "Passed " ++ show score ++ " of " ++ show (length tests)
