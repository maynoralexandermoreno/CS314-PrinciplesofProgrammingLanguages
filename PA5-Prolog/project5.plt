:- include(project5).

:- begin_tests(height).

test(1, [true(N = 0)]) :- height(tip, N).
test(2, [true(N = 2)]) :- height(bin(tip, 5, bin(tip, x, tip)), N).
test(3, [true(N = 3)]) :- height(bin(bin(bin(tip, x, tip), a, tip), 15, bin(tip, x, tip)), N).
test(4, [fail]) :- height(bin(1,2,3), _).

test(5, [fail]) :- height(bin(tip,tip,bin(bin(tip,tip),5,tip)), _).

:- end_tests(height).


:- begin_tests(bst).

test(1) :- bst(tip).
test(2) :- bst(bin(tip, 3, bin(tip, 4, tip))).
test(3, [fail]) :- bst(bin(tip, 3, bin(tip, 2, tip))).

test(4) :- bst(bin(bin(tip,1,bin(tip,2,tip)), 3, bin(tip,4,tip))).
test(5, [fail]) :- bst(bin(bin(tip,1,bin(tip,4,tip)), 3, bin(tip,4,tip))).

:- end_tests(bst).


:- begin_tests(zip).

test(1, [true(Z == [1-a, 2-b])]) :- zip([1,2],[a,b],Z).
test(2, [true(Z == [a-1, b-X, c-y])]) :- zip([a,b,c,d],[1,X,y], Z).
test(3, [set(A-B == [[1,2]-[a,b]])]) :- length(A,2), length(B,2), zip(A, B, [1-a, 2-b]).

test(4, [true(L == [3-2, 4-1, 5-9, 6-a, e-b])]) :-
	zip([3,4,5,6,e], [2,1,9,a,b], L).

test(5, [true(L == [A-X, B-Y, C-Z])]) :- zip([A,B,C,_],[X,Y,Z],L).

:- end_tests(zip).


:- begin_tests(sorted).

test(1) :- sorted([1,2,3]).
test(2) :- sorted([]).
test(3, [fail]) :- sorted([1,2,3,2]).

test(4) :- sorted([-10,-8,-5,3,4,6,10,20,100]).
test(5, [fail]) :- sorted([1,1]).
:- end_tests(sorted).


:- begin_tests(insert).

test('1-1', [true(L==[2,3,4,5])]) :- insert(3, [2,4,5], L).
test('1-2', [true(L==[1,2,3,3])]) :- insert(3, [1,2,3], L).
test('1-3', [fail]) :- insert(3, [1,2,4], [1,2,3]).

test('2-1', [true(L == [2,4,5])]) :- insert(3, L, [2,3,4,5]).
test('2-2', [fail]) :- insert(3, _, [1,4,9]).

:- end_tests(insert).


:- begin_tests(height_bonus, [condition(clause(bonus(height), true))]).

test(1, [fail]) :- height(T, 2), T = tip.
test(2, [fail]) :- height(T, 2), T = bin(tip,_,tip).
test(3) :- height(T, 2), T = bin(tip,_,bin(tip,_,tip)).
test(4) :- height(T, 2), T = bin(bin(tip,_,tip),_,tip).
test(5, [fail]) :- height(T, 2), T = bin(bin(bin(tip,_,tip),_,tip),_,tip).

:- end_tests(height_bonus).



:- begin_tests(insert_bonus, [condition(clause(bonus(insert), true))]).

% insert(?,?,+)
test('3-1', [set(X-L == [1-[2,3], 2-[1,3], 3-[1,2]])]) :-
	insert(X, L, [1,2,3]).
test('3-2', [set(X-L == [3-[1,2,4]])]) :- insert(3, L, [1,2,X,4]).

% enforce ordering
test('4-1', [fail]) :- insert(5, [1,4,2], _).
test('4-1', [true(L == [1,3,4,2])]) :- insert(3, [1,4,2], L).
test('4-2', [true(L == [1,3,8,5])]) :- insert(4, L, [1,3,4,8,5]).

:- end_tests(insert_bonus).
