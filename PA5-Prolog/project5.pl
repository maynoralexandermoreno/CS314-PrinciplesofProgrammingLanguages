% CS 314 Fall 2019
% Programming Assignment V
% Due: December 11 at 11:00 PM
% Hand in by 3:00 AM, December 12
%
% Name: Maynor A Moreno
% NetID: mam829
%
:- use_module(library(clpfd)).
:- use_module(library(dif)).

% Part I: Binary trees
% --------------------

% We re-use the definition of binary tree used in problem set 8:

tree(tip).
tree(bin(L,_,R)) :- tree(L), tree(R).

% (Note that it is not generally necessary to use tree/1 to require an argument
% to be a tree.)

tree(tip).
tree(bin(L,_,R)) :- tree(L), tree(R).

% height(+Tree, -Int) is deterministic
% height(+Tree, +Int) is semi-deterministic
%
% The height of a tree is number of nodes in the longest path from the root to
% a leaf in the tree. 
%
% ?- height(tip, N).
% N = 0.
%
% ?- height(bin(tip, 5, bin(tip, x, tip)), N).
% N = 2.
%
% ?- height(bin(bin(bin(tip, x, tip), a, tip), 15, bin(tip, x, tip)), N).
% N = 3.
%
% ?- height(bin(1,2,3), N).
% false.
% (Because it isnt a tree.)

% Create a helper function so as to be able to call freeze
height(X,Y) :- freeze(X,height2(X,Y)), !.

%height of tip is 0, height of a binary tree is 1+ maximum height of the left of the tree and the height of the right of the tree
height2(tip,0).
height2(bin(Lefttree,_,Righttree),Height) :- height2(Lefttree,Lheight), height2(Righttree,Rheight), Height#=1 + max(Lheight,Rheight).

height2(_, _) :- false.

% bst(++TreeOfInts) is semi-deterministic

% bst/1 holds for binary trees containing integers, where each node bin(L,X,R)
% has the property that every node in L is at most X and every node in R is at
% least X. Note that bst/1 is not required to succeed for partially instantiated
% arguments.
%
% ?- bst(tip).
% true.
%
% ?- bst(bin(tip, 3, bin(tip, 4, tip))).
% true.
%
% ?- bst(bin(tip, 3, bin(tip, 2, tip))).
% false.

% Same as the haskell problem set; 
%    CheckBSTRange bot tree top checks that tree is a valid binary search tree
%    and that every value in tree is less than top and greater than bot. The
%    four clauses check that:
%        (a) the root of the subtree is greater than an optional lower bound
%        (b) the root of the subtree is less than an optional upper bound
%        (c) the left subtree is valid and contains values between the lower
%            bound and x
%        (d) the right subtree is valid and contains values between x and the
%            upper bound. 

bst(tip).
bst(bin(Leftbranch,Value,Rightbranch)) :- bsthelp(Bot, bin(Leftbranch,Value,Rightbranch), Top). %, Bot #=<Value, Value #>= Top.

bsthelp(_,tip,_).
bsthelp(Bot,bin(Left,V,Right), Top) :- Bot #=< V, V #=<Top, bsthelp(Bot,Left,V), bsthelp(V,Right,Top).


% Part II: Lists
% --------------

% zip(?List, ?List, ?ListOfPairs)
% zip(+List, +List, -List) is deterministic
% zip(-List, -List, ++List) is non-deterministic
%
% zip/3 relates three lists, where the third list contains pairs whose elements
% are the elements at the same indices of the first two lists. We will use -/2
% to represent pairs.
%
% The third list will always be as long as the shorter of the first two lists;
% additional elements in the longer list are discarded.
%
% ?- zip([1,2],[a,b],Z).
% Z = [1-a, 2-b].
%
% ?- zip([a,b,c,d], [1,X,y], Z).
% Z = [a-1, b-X, c-y] .
%
% ?- length(A,2), length(B,2), zip(A, B, [1-a, 2-b]).
% A = [1,2],
% B = [a,b] .

% Define the base case of both lists being null, and those that will stop when one of the lists are null
zip([],[],[]).
zip(_,[],[]).
zip([],_,[]).
%zip of the firsts 2 lists with the third list having the tuple of the heads is true iff zip of the tails of all three lists is true
zip([X|A],[Y|B],[X-Y|Z]) :- zip(A,B,Z).
zip(_, _, _) :- false.

% sorted(++ListOfInts) is semi-deterministic
%
% sorted/1 holds for lists of zero or more integers in ascending order.
%
% ?- sorted([1,2,3]).
% true.
%
% ?- sorted([]).
% true.
%
% ?- sorted([1,2,3,2]).
% false.

%sorted is true if the first element is less than the second element, and if the tail from B to [] is also sorted
sorted([]).
sorted([_]).
sorted([A,B|C]) :- A #< B, sorted([B|C]).
sorted(_) :- false.



% insert(+Int, ++SortedList, ?SortedListWithItem) is semi-deterministic
%
% insert/3 is a relation between an integer, a sorted list of integers, and a
% second sorted list of integers that contains the first argument and all the
% elements of the second argument.
%
% ?- insert(3, [2,4,5], L).
% L = [2,3,4,5].
%
% ?- insert(3, [1,2,3], L).
% L = [1,2,3,3].
%
% ?- insert(3, [1,2,4], [1,2,3]).
% false.
%
%
% For full credit, insert/3 should also be able to remove an element from a
% list. That is, it should also work in this mode:
%
% insert(+Int, ?SortedList, ++SortedListWithItem) is semi-deterministic
%
% ?- insert(3, L, [2,3,4,5]).
% L = [2,4,5].
%
% ?- insert(3, L, [1,4,9]).
% false.
%
% The behavior of insert/3 when given a non-sorted list is undetermined.

%inserting an int into an empty list is that int in a list
insert(A,[],[A]).
insert(A,[B],[A,B]) :- A #=< B.
insert(A,[B],[B,A]) :- B #=< A.
insert(A,[B|C],[B|D]) :- A #> B, insert(A,C,D).
insert(A,[X,B|C],[X,B|D]) :- X #< B, B #< A, insert(A,[B|C],[B,D]).

%The int A is finally less than the int at position B (with a tail  C) if and only if and only A is inserted in front of [B|C]
%insert(A,[B|C],[A,B|C]) :- A #=< B.

%if A is still greater than the int at position B in both list 2 and list 3, then if true the insert must happen later
%insert(A,[B|C],[B|D]) :- A #> B, insert(A,C,D).

insert(_, _, _) :- false.

bonus(height).
bonus(insert).
