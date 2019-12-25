NO ERROR CODES TRHOWN IN MAIN ASSIGNMENT (height, bst, zip, sorted, insert); EXTRA CREDIT bonus(height) achieved; bonus(insert) attempted
NOTE: ASSIGNMENT ALLOWS FOR CHOICEPOINTS EXCEPT FOR bonus(height).

ERROR CODE FOR EXTRA CREDIT bonus(insert).
% PL-Unit: insert_bonus ..
ERROR: /ilab/users/my389/TA314/projects/PA5/PA5/PA5_ Prolog/Moreno, Maynor(mam829)/Submission attachment(s)/project5.plt:134:
    test 4-1: must fail but succeeded in 0.00 seconds

Warning: /ilab/users/my389/TA314/projects/PA5/PA5/PA5_ Prolog/Moreno, Maynor(mam829)/Submission attachment(s)/project5.plt:135:
    PL-Unit: Test 4-1: Test succeeded with choicepoint
Warning: /ilab/users/my389/TA314/projects/PA5/PA5/PA5_ Prolog/Moreno, Maynor(mam829)/Submission attachment(s)/project5.plt:136:
    PL-Unit: Test 4-2: Test succeeded with choicepoint
..
ERROR: /ilab/users/my389/TA314/projects/PA5/PA5/PA5_ Prolog/Moreno, Maynor(mam829)/Submission attachment(s)/project5.plt:142:
    test 4-11: must fail but succeeded in 0.00 seconds

ERROR: /ilab/users/my389/TA314/projects/PA5/PA5/PA5_ Prolog/Moreno, Maynor(mam829)/Submission attachment(s)/project5.plt:143:
    test 4-12: must fail but succeeded in 0.00 seconds


EXTRA CREDIT:
To enable the extra credit tests, include one or both of the following clauses in project5.pl:

bonus(insert).
bonus(height).

1. Extensions to insert/3. Make sure insert/3 will produce every reasonable answer when the first and second arguments are variables, or when the third argument contains variables

?- insert(X, L, [1,2,3]).
X = 1, L = [2,3] ;
X = 2, L = [1,3] ;
X = 3, L = [1,2].

?- insert(3, L, [1,2,X,4]).
X = 3, L = [1,2,4].

insert/3 should fail if the second or third lists are unsorted, but should not check past the insertion point.

?- insert(5, [1,4,2], L).
false.
    
?- insert(3, [1,4,2], L).
L = [1,3,4,2].
    
?- insert(4, L, [1,3,4,8,5])
L = [1,3,8,5].



2. Extensions to height/2. Ensure that height/2 terminates when the first parameter is a variable. (Note that the following examples terminate, even if additional answers are requested.)

?- height(T, 2), T = tip.
false.
    
?- height(T, 2), T = bin(tip,_,bin(tip,_,tip)).
T = bin(tip,_,bin(tip,_,tip)).
    
?- height(T, 2), T = bin(bin(tip,_,tip),_,tip).
T = bin(bin(tip,_,tip),_,tip).

Note that enabling bonus(height) may cause the test cases to fail to terminate, if you have not defined height/2 properly.
