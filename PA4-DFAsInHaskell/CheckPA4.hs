{-# LANGUAGE BangPatterns #-}
import Data.Function
import Data.List
import Control.Monad
import Control.Exception (catch, SomeException(..), evaluate)
import System.Timeout (timeout)

import Project4 (ABC(..), DFA(..))
import qualified Project4 as P


tests =
    [ ( "threeA"
      , check_initial threeA
        : check_transit threeA
        : testDFA threeA
            [ [A,B,A,A]
            , [A,A,A]
            , [C,B,A,B,C,A,B,C,A,B,C]
            , [B,B,B,B,B,A,C,C,C,B,B,A,B,B,C,C,C,A]
            ]
            [ [A,A]
            , [A,A,A,A]
            , []
            , [B,C,B,C,A,C,B,C,B]
            ]
      )
    , ( "abcs"
      , check_initial abcs
        : check_transit abcs
        : testDFA abcs
            [ [B,C,A,B,C,A,B]
            , []
            , [C,A,B,C]
            , [A,B]
            ]
            [ [A,B,B,C]
            , [C,A,B,B]
            , [A,C]
            , [C,A,B,A,B,C,A,B,C,A,B,C]
            ]
      )
    , ( "aborba"
      , check_initial aborba
        : check_transit aborba
        : testDFA aborba
            [ [C,B,C,A,B,C,C,A]
            , [A,A,A,A,B]
            , [C,C,A,C,B,A,C,C,A,A]
            , [A,B,C,C,C,C,C,C,C]
            ]
            [ [A,C,C,B,C,A]
            , []
            , [C,A,C,B,C]
            , [A,A,C,B,B]
            ]
      )
    ]

---

threeA, abcs, aborba :: DFA Int ABC
threeA = P.threeA
abcs = P.abcs
aborba = P.aborba


---

check_initial :: Eq state => DFA state symbol -> IO Bool
check_initial d
    | initial d `elem` states d = return True
    | otherwise = do
        putStrLn "Initial state not in list of states"
        putStrLn ""
        return False

check_transit :: (Eq state, Show state, Show symbol) => DFA state symbol -> IO Bool
check_transit d = check `catch` \(SomeException e) -> do
    putStrLn $ "Caught exception " ++ show e
    return False

    where
    errors = do
        s <- states d
        x <- alphabet d
        Just t <- [transit d s x]
        guard $ not (t `elem` states d)
        return $ concat ["  ", show s, " -", show x, "-> ", show t]

    check = do
        errorlist <- timeout timelimit (evaluate (rnf errors))
        case errorlist of
            Nothing -> do
                putStrLn "Timed out."
                return False
            Just [] -> return True
            Just errs -> do
                mapM_ putStrLn $ "Invalid transitions:" : errs
                putStrLn ""
                return False

-- ensure that a list gets fully evaluated
rnf l = length l `seq` l


testDFA :: Show symbol => DFA state symbol -> [[symbol]] -> [[symbol]] -> [IO Bool]
testDFA p yes no
    =  [ test (show str) (P.accepts p str) True  | str <- yes ]
    ++ [ test (show str) (P.accepts p str) False | str <- no ]

---

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
