module Project4 where

import Control.Monad


-- In this project, you will give some DFAs as values in the DFA type defined
-- below. Note that this type differs slightly from the one defined in class,
-- because the transition function may return Nothing, to indicate the absence
-- of a transition (i.e., that the machine will transition to an implicit
-- failure state for that combination of state and input).

data DFA state symbol = DFA
    { alphabet :: [symbol]
    , states   :: [state]
    , initial  :: state
    , transit  :: state -> symbol -> Maybe state
    , accept   :: state -> Bool
    }

-- This version of accepts uses foldM to halt processing of the input string
-- when the DFA transitions to the failure state. For our purposes, we have
--
--    foldM :: (b -> a -> Maybe b) -> b -> [a] -> Maybe b
--
-- foldM will use the transition function to process the input string until
-- it exhausts the string or the transition function returns Nothing.
--
-- The function maybe :: b -> (a -> b) -> Maybe a -> b allows us to apply
-- a function to the result of foldM, or return a default value when foldM
-- returns Nothing.

accepts :: DFA state symbol -> [symbol] -> Bool
accepts dfa = maybe False (accept dfa) . foldM (transit dfa) (initial dfa)

-- For example:

data ABC = A | B | C deriving (Show, Eq, Ord)

-- A DFA for the language over {A,B,C} containing strings where A occurs
-- exactly once.

oneA :: DFA Int ABC
oneA = DFA
    { alphabet = [A,B,C]
    , states   = [0,1]
    , initial  = 0
    , transit  = delta
    , accept   = (== 1)
    }
    where
    delta 0 A = Just 1
    delta 0 _ = Just 0
    delta 1 A = Nothing
    delta 1 _ = Just 1




-- A DFA for the language over {A,B,C} containing strings where A occurs
-- exactly three times.
--
-- > accepts threeA [A,B,A,A]
-- True

threeA :: DFA Int ABC
threeA = DFA
	{ alphabet = [A,B,C]
	, states =[0,1,2,3]
	, initial = 0
	, transit = delta
	, accept = (==3)
	}
	where
	delta 0 A = Just 1
	delta 0 _ = Just 0
	delta 1 A = Just 2
	delta 1 _ = Just 1
	delta 2 A = Just 3
	delta 2 _ = Just 2
	delta 3 A = Nothing
	delta 3 _ = Just 3
	delta _ _ = Nothing






-- A DFA for the language over {A,B,C} where every non-final A is followed by
-- B, every non-final B is followed by C, and every non-final C is followed by
-- A.
--
-- > accepts abcs [B,C,A,B,C,A,B]
-- True
-- > accepts abcs []
-- True
-- > accepts abcs [A,B,B,C]
-- False

abcs :: DFA Int ABC
abcs = DFA
	{ alphabet = [A,B,C]
	, states =[0,1,2,3]
	, initial = 0
	, transit = delta
	, accept = (<= 3)
	}
	where
	delta 0 A = Just 1
	delta 0 B = Just 2
	delta 0 C = Just 3
	delta 1 B = Just 2
	delta 2 C = Just 3
	delta 3 A = Just 1
	delta 1 _ = Nothing
	delta 2 _ = Nothing
	delta 3 _ = Nothing
	delta _ _ = Nothing


-- A DFA for the language over {A,B,C} of strings containing at least one of the
-- sequences "AB" or "BA".
--
-- > accepts aborba [C,B,C,A,B,C,C,A]
-- True
-- > accepts aborba [A,A,A,A,B]
-- True
-- > accepts aborba [A,C,C,B,C,A]
-- False

aborba :: DFA Int ABC
aborba = DFA
	{ alphabet = [A,B,C]
	, states =[0,1,2,3]
	, initial = 0
	, transit = delta
	, accept = (== 3)
	}
	where
	delta 0 A = Just 1
	delta 0 B = Just 2
	delta 0 C = Just 0
	delta 1 A = Just 1
	delta 1 B = Just 3
	delta 1 C = Just 0
	delta 2 A = Just 3
	delta 2 B = Just 2
	delta 2 C = Just 0
	delta 3 A = Just 3
	delta 3 B = Just 3
	delta 3 C = Just 3
	delta _ _ = Nothing
