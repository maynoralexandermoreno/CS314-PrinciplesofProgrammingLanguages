{-# LANGUAGE BangPatterns #-}
module Main where

-- Do not post this code anywhere on-line or share with students outside this
-- class.
--
-- Note that this test code uses the QuickCheck package, which is not always
-- installed by default.

import System.Timeout
import Test.QuickCheck
import Control.Applicative
import Control.Exception (catch, evaluate, SomeException)
import Control.Monad
import Data.Function
import Data.List
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

prop_paths :: Tree Char -> Bool
prop_paths t = paths t == ref_paths t

heights_tests =
    [ test_heights (Tip :: Tree Int) Tip
    , test_heights (Bin Tip 'x' Tip) (Bin Tip (1,'x') Tip)
    , test_heights t1 (Bin (Bin Tip (1,'a') Tip) (3,'b') (Bin Tip (2,'c') (Bin Tip (1,'d') Tip)))
    , test_heights t2 (Bin (Bin (Bin Tip (1,'x') Tip) (2,'y') Tip) (4,'z') (Bin (Bin Tip (1,'a') Tip) (3,'b') (Bin Tip (2,'c') (Bin Tip (1,'d') Tip))))
    , test_heights t3 (Bin (Bin (Bin Tip (1,2) Tip) (2,3) (Bin Tip (1,1) Tip)) (3,7) (Bin (Bin Tip (1,9) Tip) (2,10) (Bin Tip (1,8) Tip)))
    ]

prop_heights :: Tree Char -> Bool
prop_heights t = heights t == ref_heights t

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

prop_codes (H t) = codes t == ref_codes t

test_code =
    Fork
        (Fork
            (Leaf 'a')
            (Fork
                (Leaf 'b')
                (Leaf 'c')))
        (Fork
            (Fork
                (Fork
                    (Leaf 'd')
                    (Leaf 'e'))
                (Fork
                    (Leaf 'f')
                    (Leaf 'g')))
            (Fork
                (Leaf 'h')
                (Fork
                    (Fork
                        (Leaf 'i')
                        (Leaf 'j'))
                    (Leaf 'k'))))

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

prop_decode d (NonEmpty t) = decode d t == ref_decode d t

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
    [ ( "paths", paths_tests, [property prop_paths] )
    , ( "heights", heights_tests, [property prop_heights] )
    , ( "codes", codes_tests, [property prop_codes] )
    , ( "decode"
      , decode_tests
      , [ property (prop_decode (Fork aeiou_code P.xyz_code))
        , property (prop_decode test_code)
        , property (\(H t) -> prop_decode t)
        ]
      )
    ]

-- reference implementations

ref_paths :: Tree a -> [[a]]
ref_paths Tip = []
ref_paths (Bin Tip x Tip) = [[x]]
ref_paths (Bin l x r) = fmap (x:) (ref_paths l ++ ref_paths r)

ref_heights :: Tree a -> Tree (Integer, a)
ref_heights = fst . go
    where
    go Tip = (Tip, 0)
    go (Bin l x r) = (Bin l' (h, x) r', h)
        where
        (l', hl) = go l
        (r', hr) = go r
        h = 1 + max hl hr

ref_codes :: BTree a -> [(a, [Bool])]
ref_codes (Leaf x) = [(x, [])]
ref_codes (Fork l r) = map (fmap (False:)) (ref_codes l) ++ map (fmap (True:)) (ref_codes r)

ref_decode :: BTree a -> [Bool] -> ([a], Bool)
ref_decode code [] = ([],True)
ref_decode code bs = go code bs
    where
    go (Fork l r) [] = ([], False)
    go (Fork l r) (False:bs) = go l bs
    go (Fork l r) (True:bs) = go r bs
    go (Leaf x) bs = (x:xs, c)
        where
        (xs, c) = ref_decode code bs



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

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized (arbTree arbitrary)

    shrink Tip = []
    shrink (Bin l x r)
        =  [Tip]
        ++ [ Bin sl x r | sl <- shrink l ]
        ++ [ Bin l x sr | sr <- shrink r ]
        ++ [ Bin l sx r | sx <- shrink x ]

arbTree item n
    | n < 1 = pure Tip
    | otherwise = oneof
        [ pure Tip
        , Bin <$> arbTree item h <*> item <*> arbTree item h
        ]
        where h = n `div` 2


newtype H = H (BTree Char) deriving (Show, Eq)

instance Arbitrary H where
    arbitrary = fmap H . sized $ \i -> arbBTree (take (i+2) ['a'..'z'])

arbBTree [x] = pure $ Leaf x
arbBTree xs = do
    let n = length xs
    i <- choose (1, length xs - 1)
    let (y,z) = splitAt i xs
    Fork <$> arbBTree y <*> arbBTree z


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

args = stdArgs { maxSuccess = 500, maxSize = 100 }


runProps :: [Property] -> IO Integer
runProps = foldM go 0
    where
    go s p = do
        r <- quickCheckWithResult args (within timelimit p)
        case r of
            Success{} -> return (s + 1)
            _ -> return s

runTestGroups !score [] = return score
runTestGroups !score ((name, tests, props) : groups) = do
    putStrLn $ "\nTesting " ++ name
    tscore <- runTests tests
    putStrLn $ "\nUnit tests passed: " ++ show tscore ++ " / " ++ show (length tests)
    pscore <- runProps props
    let pscore' = (pscore * genericLength tests) `div` genericLength props
    putStrLn $ "\nProperty test score: " ++ show pscore'
    runTestGroups (score + tscore + pscore') groups

main = do
    score <- runTestGroups 0 tests
    extra <- if P.extra_credit then runTests lazy_decode_tests else return 0
    putStrLn $ "\nScore: " ++ show score
    putStrLn $ "Extra Credit: " ++ show extra
