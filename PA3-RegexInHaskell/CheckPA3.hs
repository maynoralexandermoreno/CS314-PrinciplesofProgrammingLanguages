{-# LANGUAGE BangPatterns #-}
import Data.Function
import Data.List
import Control.Monad
import Control.Exception (catch, SomeException, evaluate)
import System.Timeout (timeout)

import Project3 (ABC(..), RE(..))
import qualified Project3 as P


-- Change this to True to enable testing regular expressions using the
-- matchDeriv defined in Project3, instead of the match defined in this module.
-- Using matchDeriv provides additional testing for matchEmpty, and may be
-- faster.

use_matchDeriv = False




tests =
    [ ( "one"
      , testRegex one
            [ [C]
            , [A,B,C]
            , [C,A,B]
            , [A,B,C,B,A]
            , [A,B,A,A,C,A,B,A]
            ]
            [ []
            , [C,C]
            , [A,B,C,A,B,C]
            , [C,C,C,C,C]
            , [A,B,A,A,B,C,A,C]
            ]
      )
    , ( "two"
      , testRegex two
            [ []
            , [A,A]
            , [A,B,C,A,B,C]
            , [C,A,B,A,A,C,B,B,C,A]
            , [B,C,B]
            ]
            [ [A]
            , [C,A,B,B,C,B]
            , [A,B,C,A,B,C,A,B,C]
            , [B,A,C,C,C,B,A,A,C]
            , [A,A,A,A,A,A,A]
            ]
      )
    , ( "three"
      , testRegex three
            [ []
            , [A,B]
            , [A,B,C,A,B,C]
            , [C,B,B,A,B,B,B,C]
            , [B,C,B]
            ]
            [ [A]
            , [C,A,B,A,A,C,B,B,C,A]
            , [B,A,C,C,C,B,A,A,C]
            , [A,A,A,A,A,A,A]
            , [A,B,A,B,A,B,A,C]
            ]
      )
    , ( "matchEmpty"
      , [ test "e" (matchEmpty (Empty :: RE ABC)) True
        , test "B" (matchEmpty b) False
        , test "e|A" (matchEmpty (Empty :+ a)) True
        , test "A*" (matchEmpty (Repeat a)) True
        , test "A*C" (matchEmpty (Repeat a :* c)) False
        , test "(A|e)+" (matchEmpty (Plus (a :+ Empty))) True
        , test "(AB)*" (matchEmpty (Repeat (a :* b))) True
        , test "0" (matchEmpty Never) False
        , test "A|0" (matchEmpty (a :+ Never)) False
        , test "eA*" (matchEmpty (Empty :+ Repeat a)) True
        ]
      )
    , ( "firsts"
      , [ testSet "e" (firsts (Empty :: RE ABC)) []
        , testSet "B" (firsts b) [B]
        , testSet "e|A" (firsts (Empty :+ a)) [A]
        , testSet "A|B" (firsts (a :+ b)) [A,B]
        , testSet "A*B" (firsts (Repeat a :* b)) [A,B]
        , testSet "(A|B)*C" (firsts (Repeat (a :+ b) :* c)) [A,B,C]
        , testSet "(wx)|(yz)" (firsts (w :* x :+ y :* z)) "wy"
        , testSet "(wx)*yz" (firsts (Repeat (w :* x) :* y :* z)) "wy"
        , testSet "w*x*yz" (firsts (Repeat w :* Repeat x :* y :* z)) "wxy"
        , testSet "x(y|z)" (firsts (x :* (y :+ z))) "x"
        ]
      )
    ]

[a,b,c] = map Symbol [A,B,C]
[w,x,y,z] = map Symbol "wxyz"

match :: Eq s => RE s -> [s] -> Bool
match = if use_matchDeriv then P.matchDeriv else matchSlow

matchSlow :: Eq s => RE s -> [s] -> Bool
matchSlow Never _ = False
matchSlow Empty [] = True
matchSlow Empty _ = False
matchSlow (Symbol s) [x] = s == x
matchSlow (Symbol _) _ = False
matchSlow (p :+ q) xs = matchSlow p xs || matchSlow q xs
matchSlow (p :* q) xs
    = any (\(u,v) -> matchSlow p u && matchSlow q v) (splits xs)
matchSlow (Repeat p) [] = True
matchSlow (Repeat p) (x:xs)
    = any (\(u,v) -> matchSlow p (x:u) && matchSlow (Repeat p) v) (splits xs)
matchSlow (Plus p) xs = matchSlow (p :* Repeat p) xs

splits []     = [([],[])]
splits (x:xs) = ([], x:xs) : [ (x:u, v) | (u,v) <- splits xs ]


-- enforce types


one, two, three :: RE ABC
one = P.one
two = P.two
three = P.three

matchEmpty :: RE a -> Bool
matchEmpty = P.matchEmpty

firsts :: RE a -> [a]
firsts = P.firsts

-- test generators

testRegex :: (Eq a, Show a) => RE a -> [[a]] -> [[a]] -> [IO Bool]
testRegex p yes no =
    [ test (show r) (match p r) True  | r <- yes ] ++
    [ test (show r) (match p r) False | r <- no ]

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

testSet :: (Ord a, Show a) => String -> [a] -> [a] -> IO Bool
testSet = testBy ((==) `on` nub . sort)

runTests :: [IO Bool] -> IO Int
runTests = foldM (\s -> fmap (\b -> if b then s+1 else s)) 0

runTestGroups !score [] = return score
runTestGroups !score ((name, tests) : groups) = do
    putStrLn $ "\nTesting " ++ name
    tscore <- runTests tests
    putStrLn $ "\nUnit tests passed: " ++ show tscore ++ " / " ++ show (length tests)
    runTestGroups (score + tscore) groups

main = do
    score <- runTestGroups 0 tests
    putStrLn $ "\nScore: " ++ show (score `div` 2)
