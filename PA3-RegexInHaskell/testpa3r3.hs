{-# LANGUAGE BangPatterns #-}
import Data.Function
import Data.List
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import Control.Exception (catch, SomeException, evaluate)
import System.Timeout (timeout)
import Test.QuickCheck

import Project3 (ABC(..), RE(..))
import qualified Project3 as P


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
      , [ property $ \str -> match one str == (length (filter (C ==) str) == 1) ]
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
      , [ property $ \str -> match two str == even (length (filter (A ==) str)) ]
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
      , [ property $ \str -> match three str ==
                all (not . (`elem` [[A],[A,A],[A,C]]) . take 2) (tails str) ]
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
        , test "eA*" (matchEmpty (Empty :* Repeat a)) True
        ]
      , [ property $ \r -> matchEmpty r == matchEmpty (r :: RE Letter) ]
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
      , [ property $ \(NotNever p) -> firsts p =~= ref_firsts (p :: RE Letter)
        , property $ \p -> firsts p =~= ref_firsts (p :: RE Letter)
        ]
      )
    ]

[a,b,c] = map Symbol [A,B,C]
[w,x,y,z] = map Symbol "wxyz"

match :: (Ord sym) => RE sym -> [sym] -> Bool
match p = thd . foldl' tderiv (trie p)
    where
    tderiv (_ :< t) x = Map.findWithDefault tzero x t


data Trie a = Bool :< Map.Map a (Trie a)
thd (b :< _) = b
ttl (_ :< t) = t

tzero = False :< Map.empty
tone  = True :< Map.empty

infixr 6 .+.
infixr 7 .*.

(.+.) :: Ord a => Trie a -> Trie a -> Trie a
(a :< dp) .+. (b :< dq) = (a || b) :< Map.unionWith (.+.) dp dq

(.*.) :: Ord a => Trie a -> Trie a -> Trie a
(True  :< dp) .*. q = q .+. False :< fmap (.*. q) dp
(False :< dp) .*. q = False :< fmap (.*. q) dp

tstar :: (Ord a) => Trie a -> Trie a
tstar (_ :< dp) = q where q = True :< fmap (.*. q) dp

trie :: Ord a => RE a -> Trie a
trie Never      = tzero
trie Empty      = tone
trie (Symbol x) = False :< Map.singleton x tone
trie (p :+ q)   = trie p .+. trie q
trie (p :* q)   = trie p .*. trie q
trie (Repeat p) = tstar (trie p)
trie (Plus p)   = let t = trie p in t .*. tstar t


-- enforce types


one, two, three :: RE ABC
one = P.one
two = P.two
three = P.three

matchEmpty :: RE a -> Bool
matchEmpty = P.matchEmpty

firsts :: RE a -> [a]
firsts = P.firsts

-- reference implementations

ref_matchEmpty :: RE sym -> Bool
ref_matchEmpty Never = False
ref_matchEmpty Empty = True
ref_matchEmpty (Symbol _) = False
ref_matchEmpty (p :+ q) = ref_matchEmpty p || ref_matchEmpty q
ref_matchEmpty (p :* q) = ref_matchEmpty p && ref_matchEmpty q
ref_matchEmpty (Repeat _) = True
ref_matchEmpty (Plus p) = ref_matchEmpty p


ref_firsts :: RE sym -> [sym]
ref_firsts = fst . go
    where
    go Never = ([], False)
    go Empty = ([], True)
    go (Symbol x) = ([x], False)
    go (p :+ q) = (fp ++ fq, ep || eq)
        where
        (fp, ep) = go p
        (fq, eq) = go q
    go (p :* q)
        | null fq && not eq = ([], False)
        | ep                = (fp ++ fq, ep && eq)
        | otherwise         = (fp, ep && eq)
        where
        (fp, ep) = go p
        (fq, eq) = go q
    go (Repeat p) = (fst (go p), True)
    go (Plus p) = go p

-- test generators

testRegex :: (Ord a, Show a) => RE a -> [[a]] -> [[a]] -> [IO Bool]
testRegex p yes no =
    [ test (show r) (match p r) True  | r <- yes ] ++
    [ test (show r) (match p r) False | r <- no ]

instance Arbitrary ABC where
    arbitrary = oneof [pure A, pure B, pure C]


newtype Letter = Letter Char deriving (Eq, Ord)

instance Show Letter where
    showsPrec p (Letter c) = showsPrec p c

instance Arbitrary Letter where
    arbitrary = sized $ \n ->
            fmap Letter . oneof . map pure $ take (n+1) letters
        where
        letters = ['a'..'z'] ++ ['A'..'Z']

    shrink (Letter x) = Letter <$> shrink x

instance Arbitrary a => Arbitrary (RE a) where
    arbitrary = sized (arbRE arbitrary)

    shrink (Symbol x) = Symbol <$> shrink x
    shrink (p :+ q) = [p,q]
        ++ [p' :+ q | p' <- shrink p]
        ++ [p :+ q' | q' <- shrink q]
    shrink (p :* q) = [p,q]
        ++ [p' :* q | p' <- shrink p]
        ++ [p :* q' | q' <- shrink q]
    shrink (Repeat p) = p : (Repeat <$> shrink p)
    shrink (Plus p) = p : Repeat p : (Plus <$> shrink p)
    shrink _ = []


arbRE :: Gen a -> Int -> Gen (RE a)
arbRE g 0 = oneof [pure Never, pure Empty, Symbol <$> g]
arbRE g n = oneof
    [ pure Never
    , pure Empty
    , Symbol <$> g
    , (:+) <$> arbRE g h <*> arbRE g h
    , (:*) <$> arbRE g h <*> arbRE g h
    , Repeat <$> arbRE g m
    , Plus <$> arbRE g m
    ]
    where
    m = n - 1
    h = n `div` 2


newtype NotNever a = NotNever (RE a)

instance Show a => Show (NotNever a) where
    showsPrec p (NotNever r) = showsPrec p r

instance Arbitrary a => Arbitrary (NotNever a) where
    arbitrary = NotNever <$> sized (arbRENN arbitrary)

    shrink (NotNever p) = NotNever <$> shrink p


arbRENN :: Gen a -> Int -> Gen (RE a)
arbRENN g 0 = oneof [pure Empty, Symbol <$> g]
arbRENN g n = oneof
    [ pure Empty
    , Symbol <$> g
    , (:+) <$> arbRENN g h <*> arbRENN g h
    , (:*) <$> arbRENN g h <*> arbRENN g h
    , Repeat <$> arbRENN g m
    , Plus <$> arbRENN g m
    ]
    where
    m = n - 1
    h = n `div` 2


--

timelimit = 100*1000000

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

(=~=) :: (Ord a) => [a] -> [a] -> Bool
(=~=) = (==) `on` nub . sort

testSet :: (Ord a, Show a) => String -> [a] -> [a] -> IO Bool
testSet = testBy (=~=)

runTests :: [IO Bool] -> IO Int
runTests = foldM (\s -> fmap (\b -> if b then s+1 else s)) 0

args = stdArgs { maxSuccess = 500, maxSize = 100 }


runProps :: [Property] -> IO Int
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
    let tlen = length tests
    putStrLn $ "\nUnit tests passed: " ++ show tscore ++ " / " ++ show tlen
    pscore <- runProps props
    let plen = length props
    putStrLn $ "\nProperty tests passed: " ++ show pscore ++ " / " ++ show plen
    let pscore' = (pscore * tlen) `div` plen
    runTestGroups (score + tscore + pscore') groups

main = do
    score <- runTestGroups 0 tests
    putStrLn $ "\nScore: " ++ show (score `div` 2)
