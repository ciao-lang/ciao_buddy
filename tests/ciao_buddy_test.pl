:- module(ciao_buddy_test,[],[assertions]).

:- use_module(ciao_buddy(ciao_buddy)).
:- use_module(library(streams)).
:- use_module(library(write)).

% #define bddop_and       0
% #define bddop_xor       1
% #define bddop_or        2
% #define bddop_nand      3
% #define bddop_nor       4
% #define bddop_imp       5
% #define bddop_biimp     6
% #define bddop_diff      7
% #define bddop_less      8
% #define bddop_invimp    9

:- test test/0.
:- export(test/0).
test :-
    bdd_init(1000,100,_),
    bdd_setvarnum(20,_),
    bdd_ithvar(0,X),
    bdd_ithvar(0,A),
    bdd_ithvar(1,Y),
    bdd_apply(X,Y,5,Z), % Z = X & Y
    bdd_addref(Z,Z),
    bdd_printtable(Z),
    bdd_apply(A,Y,5,ZZ), % ZZ = A & Y
    bdd_addref(ZZ,ZZ),
    bdd_printtable(ZZ),
    % now rename the variables in Z, using subst {X/U, Y/V}
    bdd_newpair(Subst),
    bdd_setpair(Subst,0,2,_),
    bdd_setpair(Subst,1,3,_),
    bdd_replace(Z,Subst,W),
    bdd_addref(W,W),
    bdd_printtable(W),
    bdd_printtable(Z),
    bdd_delref(Z,_),
    bdd_delref(W,_),
    bdd_nithvar(0,X4),
    bdd_apply(X,X4,0,Z1),
    bdd_printtable(Z1),
    bdd_false(Z1),
    write('All done'),nl,
    bdd_done(0).
