:- module(ciao_buddy,[
			bdd_init/3,
			bdd_done/1,
			bdd_setvarnum/2,
			bdd_ithvar/2,
			bdd_nithvar/2,
			bdd_apply/4,
			bdd_addref/2,
			bdd_delref/2,
			bdd_exists/3,
			bdd_makeset/3,
			bdd_newpair/1,
			bdd_setpair/4,
			bdd_replace/3,
			bdd_false/1,
			bdd_true/1,
			bdd_not/2,
			bdd_ite/4,
			bdd_high/2,
			bdd_low/2,
			bdd_and/3,
			bdd_or/3,
			bdd_printtable/1,
			bdd_build/2,
			bdd_build1/3
            ],
            [assertions,foreign_interface]).

% Ciao Prolog foreign language interface for BDD package Buddy 2.4
% Author: John Gallagher

:- trust pred bdd_init(in(_),in(_),go(R)) :: 
					c_int * c_int * c_int + (foreign, returns(R)).
					
:- trust pred bdd_done(in(_)) :: 
					c_int + foreign.
					
:- trust pred bdd_setvarnum(in(_), go(R)) :: 
                  c_int * c_int + (foreign, returns(R)).

:- trust pred bdd_ithvar(in(_), go(BDD)) :: 
                  c_int * c_int + (foreign, returns(BDD)).

:- trust pred bdd_nithvar(in(_), go(BDD)) :: 
                  c_int * c_int + (foreign, returns(BDD)).

:- trust pred bdd_apply(in(_), in(_), in(_), go(BDD)) :: 
                  c_int * c_int * c_int * c_int + (foreign, returns(BDD)).

:- trust pred bdd_addref(in(_), go(BDD)) :: 
                  c_int * c_int + (foreign, returns(BDD)).

:- trust pred bdd_delref(in(_), go(BDD)) :: 
                  c_int * c_int + (foreign, returns(BDD)).
                  
:- trust pred bdd_makeset(in(_), in(_), go(BDD)) :: 
                  c_int * c_int * c_int + (foreign, returns(BDD)).
                
:- trust pred bdd_exists(in(_), in(_), go(BDD)) :: 
                  c_int * c_int * c_int + (foreign, returns(BDD)).
                  
:- trust pred bdd_newpair(go(Pair)) ::
				  address + (foreign, returns(Pair)).
                  
:- trust pred bdd_setpair(in(_), in(_), in(_), go(R)) ::
				  address * c_int * c_int * c_int + (foreign, returns(R)).
				  
:- trust pred bdd_replace(in(_), in(_), go(R)) ::
				  c_int * address * c_int + (foreign, returns(R)).
				  
:- trust pred bdd_false(go(BDD)) ::
				  c_int + (foreign, returns(BDD)).

:- trust pred bdd_true(go(BDD)) ::
				  c_int + (foreign, returns(BDD)).
				  
:- trust pred bdd_not(in(_),go(BDD)) ::
				  c_int * c_int + (foreign, returns(BDD)).
				  
:- trust pred bdd_ite(in(_), in(_), in(_), go(BDD)) :: 
                  c_int * c_int * c_int * c_int + (foreign, returns(BDD)).
                  
:- trust pred bdd_high(in(_), go(BDD)) ::
				  c_int * c_int + (foreign, returns(BDD)).

:- trust pred bdd_low(in(_), go(BDD)) ::
				  c_int * c_int + (foreign, returns(BDD)).
              
:- trust pred bdd_and(in(_), in(_), go(BDD)) :: 
                  c_int * c_int * c_int + (foreign, returns(BDD)).

:- trust pred bdd_or(in(_), in(_), go(BDD)) :: 
                  c_int * c_int * c_int + (foreign, returns(BDD)).
               
:- trust pred bdd_printtable(in(_)) :: 
                  c_int + foreign.

:- include(.(ciao_buddy_config_auto)).

:- use_foreign_library(bdd).

% build_bdd(E,B) builds a BDD B from the expression E.
% Syntax for expressions
% E ::= 0 | 1 | Var | -E | E1+E2 | E1*E2 | E1->E2 | E1=E2
% Var is a Prolog atom or else a term '$VAR'(N) where N is atom or number.
% E.g. bdd_build(x+(y*z),B).
% E.g. bdd_build('$VAR'(0)+('$VAR'(a)*'$VAR'(b)),B).


bdd_build(E,B) :-
	bdd_build1(E,B,_).
	
% bdd_build1(E,B,Dict) - called by bdd_build/2.
% Like bdd_build but the 3rd argument is a dictionary of
% variables of the form [(v1,0),(v2,1),....|_] where the tail is a variable.
% E.g. bdd_build1(x+(y*z),B1,Dict),bdd_build1(w->(x+y),B2,Dict).
% Assuming Dict is free before the first call.
% After the first call Dict=[(x,0),(y,1),(z,2)|Dict1]
% After the second call Dict=[(x,0),(y,1),(z,2),(w,3)|Dict2] and Dict1=[(w,3)|Dict2].

:- doc(bug, "bdd_addref for all nodes might be unnecessary and cause memory leaks.").

bdd_build1(0,B,_) :-
	bdd_false(B),
	bdd_addref(B,B).
bdd_build1(false,B,_) :-
	!,
	bdd_false(B),
	bdd_addref(B,B).
bdd_build1(1,B,_) :-
	bdd_true(B),
	bdd_addref(B,B).
bdd_build1(true,B,_) :-
	!,
	bdd_true(B),
	bdd_addref(B,B).
bdd_build1(X,B,Dict) :-
	boolvar(X),
	!,
	lookup(X,Dict,N),
	bdd_ithvar(N,B).
bdd_build1(-(X),B,Dict) :-
	bdd_build1(X,B1,Dict),
	bdd_not(B1,B),
	bdd_addref(B,B).
bdd_build1(X*Y,B,Dict) :-
	bdd_build1(X,B1,Dict),
	bdd_build1(Y,B2,Dict),
	bdd_and(B1,B2,B),
	bdd_addref(B,B).
bdd_build1(X+Y,B,Dict) :-
	bdd_build1(X,B1,Dict),
	bdd_build1(Y,B2,Dict),
	bdd_or(B1,B2,B),
	bdd_addref(B,B).
bdd_build1((X->Y),B,Dict) :-
	bdd_build1(X,B1,Dict),
	bdd_build1(Y,B2,Dict),
	bdd_apply(B1,B2,5,B),
	bdd_addref(B,B). 	% 5 is opcode for ->
bdd_build1((X=Y),B,Dict) :-
	bdd_build1(X,B1,Dict),
	bdd_build1(Y,B2,Dict),
	bdd_apply(B1,B2,6,B),
	bdd_addref(B,B).	% 6 is opcode for =
	
	
lookup(X,Dict,N) :-
	lookup1(X,0,Dict,N).
	
lookup1(X,N,[(X,N)|_],N) :-
	!.
lookup1(X,K,[_|Dict],N) :-
	K1 is K+1,
	lookup1(X,K1,Dict,N).
	
boolvar(X) :-
	atom(X),
	!.
boolvar('$VAR'(X)) :-
	atomic(X).
	
