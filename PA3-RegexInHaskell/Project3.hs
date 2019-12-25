module Project3 where

-- Modify this file by replacing the (non-)definitions of each regular
-- expression and function with working definitions. Do not modify the types of
-- the expressions or functions, as this may prevent the grading script from
-- compiling. Similarly, do not modify the definitions of RE or ABC.


import Data.List

data RE sym
    = Never             -- match no strings
    | Empty             -- match empty string
    | Symbol sym        -- match singleton string
    | RE sym :+ RE sym  -- choice
    | RE sym :* RE sym  -- concatenation
    | Repeat (RE sym)   -- repeat zero or more times
    | Plus (RE sym)     -- repeat one or more times
    deriving (Show, Eq)

infixr 6 :+, .+.
infixr 7 :*, .*.

data ABC = A | B | C deriving (Show, Eq, Ord)


-- 1
-- Create a regular expression for the language over ABC containing strings
-- with exactly one C

one :: RE ABC
one = (Repeat(Symbol A :+ Symbol B)) :* (Symbol C) :* (Repeat(Symbol A :+ Symbol B))



-- 2
-- Create a regular expression for the language over ABC containing strings
-- with an even number of A's

two :: RE ABC
two = Repeat(Repeat(Symbol B :+ Symbol C) :* (Symbol A) :* Repeat(Symbol B :+ Symbol C) :* (Symbol A) ):* (Repeat(Symbol B :+ Symbol C))


-- 3
-- Create a regular expression for the language over ABC containing strings
-- where every A is immediately followed by a B

three :: RE ABC
three = Repeat(Repeat(Symbol B :+ Symbol C) :* Repeat((Symbol A :* Symbol B)) :* Repeat(Symbol B :+ Symbol C))

-- 4
-- Write a function matchEmpty that returns true for regular expressions that
-- match the empty string.

matchEmpty :: RE sym -> Bool
matchEmpty Never = False
matchEmpty Empty = True
matchEmpty (Empty :+ _) = True
matchEmpty (_ :+ Empty) = True
matchEmpty (Empty :* _) = True
matchEmpty (_ :* Empty) = True
matchEmpty (Repeat _) = True
matchEmpty (Plus x) = matchEmpty x 
matchEmpty _ = False



-- 5
-- Write a function firsts that, for a regular expression p, returns a list of
-- symbols such that (1) every string matching p begins with a symbol that
-- occurs somewhere in firsts p, and (2) every symbol occuring in firsts p
-- appears at the beginning of some string in the language of p.
--
-- Note that the symbol type is completely polymorphic, so it is not possible
-- to sort the list or remove duplicates. Note also that the list must be
-- finite, even if the number of strings in the language is infinite.

firsts :: RE sym -> [sym]
first (Never) = []
firsts (Symbol a) = [a]
firsts (Empty) = []
firsts (Repeat a) = firsts a
firsts (Plus a) = firsts a
firsts (a :+ b) = (firsts a) ++ (firsts b)
firsts ((Symbol a) :* b) = [a]
firsts (a :* b) = (firsts a) ++ (firsts b)
--firsts (left :+ right) = [firsts left] ++ [firsts right]
--firsts (start :* next) = [firsts start]



-- utilities
-- You may use matchDeriv to check your regular expressions. Note that you
-- must define matchEmpty in order for matchDeriv to work.

matchDeriv :: (Eq sym) => RE sym -> [sym] -> Bool
matchDeriv p = matchEmpty . foldl' deriv p

deriv :: (Eq sym) => RE sym -> sym -> RE sym
deriv Never      _ = Never
deriv Empty      _ = Never
deriv (Symbol s) x
    | s == x       = Empty
    | otherwise    = Never
deriv (p :+ q)   x = deriv p x .+. deriv q x
deriv (p :* q) x
    | matchEmpty p = deriv q x .+. deriv p x .*. q
    | otherwise    = deriv p x .*. q
deriv (Repeat p) x = deriv p x .*. Repeat p
deriv (Plus p)   x = deriv p x .*. Repeat p

-- Alternative forms of :+ and :* that perform some elementary simplification.
-- These reduce the size of the expressions deriv produces in many common cases.

(.+.) :: RE sym -> RE sym -> RE sym
Never .+. q     = q
p     .+. Never = p
p     .+. q     = p :+ q

(.*.) :: RE sym -> RE sym -> RE sym
Never .*. q = Never
Empty .*. q = q
p     .*. q = p :* q
