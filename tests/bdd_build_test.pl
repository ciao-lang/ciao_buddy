:- module(bdd_build_test, [], [assertions]).

:- use_module(ciao_buddy(ciao_buddy)).

:- export(test/1).
test(E) :-
    bdd_init(1000,100,_),
    bdd_setvarnum(20,_),
    bdd_build(E,B),
    bdd_printtable(B),
    bdd_done(0).

:- test test1/0.
:- export(test1/0).
test1 :-
    test(a*(b+c)).

:- test test2/0.
:- export(test2/0).
test2 :-
    test('$VAR'(0)*('$VAR'(1)+'$VAR'(2))).

:- test test3/0.
:- export(test3/0).
test3 :-
    test((p->(q->p))).

:- test test4/0.
:- export(test4/0).
test4 :-
    test((x=1)*(x=0)).