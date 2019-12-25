{-# LANGUAGE BangPatterns #-}

-- To use this test script, place it in the same directory as your Project2.hs
-- file, load it into GHCi, and enter main at the prompt.
--
-- Passing the 20 tests provided here is sufficient to get 50% credit for this
-- project. To get the remaining points, your functions must be correct for all
-- sensible inputs. (For example, codes and decode will only be given valid
-- Huffman trees.)
--
-- During development, you may comment out tests for functions you have not
-- yet completed. You may add additional tests of your own, but these tests
-- will not contribute directly towards your score.


module CheckPA2 where

import Control.Monad
import Control.Exception (catch, SomeException, evaluate)
import System.Timeout (timeout)
import Data.List (sortBy)
import Data.Function (on)

import Project2 (Tree(..), BTree(..))
import qualified Project2 as P

-- tests

t1 = Bin (Bin Tip 'a' Tip) 'b' (Bin Tip 'c' (Bin Tip 'd' Tip))

t2 = Bin (Bin (Bin Tip 'x' Tip) 'y' Tip) 'z' t1

t3 :: Tree Int
t3 = Bin (Bin (Bin Tip 2 Tip) 3 (Bin Tip 1 Tip)) 7 (Bin (Bin Tip 9 Tip) 10 (Bin Tip 8 Tip))

paths_tests =
    [ test_paths Tip ([] :: [[Integer]])
    , test_paths (Bin Tip 'a' Tip) ["a"]
    , test_paths t1 ["ba", "bcd"]
    , test_paths t2 ["zyx", "zba", "zbcd"]
    , test_paths t3 [[7,3,2],[7,3,1],[7,10,9],[7,10,8]]
    ]

heights_tests =
    [ test_heights (Tip :: Tree Int) Tip
    , test_heights (Bin Tip 'x' Tip) (Bin Tip (1,'x') Tip)
    , test_heights t1 (Bin (Bin Tip (1,'a') Tip) (3,'b') (Bin Tip (2,'c') (Bin Tip (1,'d') Tip)))
    , test_heights t2 (Bin (Bin (Bin Tip (1,'x') Tip) (2,'y') Tip) (4,'z') (Bin (Bin Tip (1,'a') Tip) (3,'b') (Bin Tip (2,'c') (Bin Tip (1,'d') Tip))))
    , test_heights t3 (Bin (Bin (Bin Tip (1,2) Tip) (2,3) (Bin Tip (1,1) Tip)) (3,7) (Bin (Bin Tip (1,9) Tip) (2,10) (Bin Tip (1,8) Tip)))
    ]

aeiou_code =
    Fork
        (Leaf 'e')
        (Fork
            (Fork (Leaf 'a') (Leaf 'i'))
            (Fork (Leaf 'o') (Leaf 'u')))


codes_tests =
    [ test_codes (Fork (Leaf True) (Leaf False)) [(True, [False]), (False, [True])]
    , test_codes P.xyz_code [('x', [False]), ('y', [True,False]), ('z',[True,True])]
    , test_codes aeiou_code
        [ ('a', [True,False,False])
        , ('e', [False])
        , ('i', [True,False,True])
        , ('o', [True,True,False])
        , ('u', [True,True,True])
        ]
    , test_codes (Fork P.xyz_code aeiou_code)
        [ ('a', [True,True,False,False])
        , ('e', [True,False])
        , ('i', [True,True,False,True])
        , ('o', [True,True,True,False])
        , ('u', [True,True,True,True])
        , ('x', [False,False])
        , ('y', [False,True,False])
        , ('z', [False,True,True])
        ]
    ]

decode_tests =
    [ test_decode (Fork (Leaf True) (Leaf False))
        [True,True,False]
        ([False,False,True], True)
    , test_decode P.xyz_code [True, False] ("y", True)
    , test_decode P.xyz_code
        [True,False,True,True,True,False,False,True,False]
        ("yzyxy", True)
    , test_decode P.xyz_code
        [True,False,True,True,True,False,False,True]
        ("yzyx", False)
    , test_decode aeiou_code
        [True,False,False,True,True,True,False]
        ("aue", True)
    , test_decode aeiou_code
        [False,False,False,False]
        ("eeee", True)
    ]

lazy_decode_tests =
    [ test "extra credit 1"
        (take 3 . fst . decode P.xyz_code $ False:False:False:error "too far")
        "xxx"
    , test "extra credit 2"
        (take 4 . fst . decode aeiou_code $
            True:False:True:True:True:False:False:True:True:False:True:False:error "too far")
        "ioeo"
    ]

tests =
    [ ( "paths", paths_tests )
    , ( "heights", heights_tests )
    , ( "codes", codes_tests )
    , ( "decode", decode_tests )
    ]

-- enforce type signatures

paths :: Tree a -> [[a]]
paths = P.paths

heights :: Tree a -> Tree (Integer, a)
heights = P.heights

codes :: BTree a -> [(a, [Bool])]
codes = P.codes

decode :: BTree a -> [Bool] -> ([a], Bool)
decode = P.decode

-- testing code

test_f1 :: (Show a, Show b) => (b -> b -> Bool) -> String -> (a -> b) -> a -> b -> IO Bool
test_f1 eq name f x r = testBy eq fx (f x) r
    where fx = (showString name . showChar ' ' . showsPrec 11 x) ""

test_paths x = test_f1 (==) "paths" paths x
test_heights x = test_f1 (==) "heights" heights x
test_codes x = test_f1 ((==) `on` sortBy (compare `on` fst)) "codes" codes x

test_decode x y r = test fxy (decode x y) r
    where fxy = showString "decode " . showsPrec 11 x
              . showChar ' ' . showsPrec 11 y $ ""

--

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

runTests :: [IO Bool] -> IO Integer
runTests = foldM (\s -> fmap (\b -> if b then s+1 else s)) 0

runTestGroups !score [] = return score
runTestGroups !score ((name, tests) : groups) = do
    putStrLn $ "\nTesting " ++ name
    tscore <- runTests tests
    putStrLn $ "\nUnit tests passed: " ++ show tscore ++ " / " ++ show (length tests)
    runTestGroups (score + tscore) groups

main = do
    score <- runTestGroups 0 tests
    extra <- if P.extra_credit then runTests lazy_decode_tests else return 0
    putStrLn $ "\nScore: " ++ show score
    when P.extra_credit $ putStrLn $ "Extra Credit: " ++ show extra
