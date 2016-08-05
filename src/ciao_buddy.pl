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
			bdd_printtable/1
            ],
            [assertions,foreign_interface]).

% Ciao Prolog foreign language interface for BDD package Buddy 2.4
% Author: John Gallagher

:- true pred bdd_init(in(_),in(_),go(R)) :: 
					c_int * c_int * c_int + (foreign, returns(R)).
					
:- true pred bdd_done(in(_)) :: 
					c_int + foreign.
					
:- true pred bdd_setvarnum(in(_), go(R)) :: 
                  c_int * c_int + (foreign, returns(R)).

:- true pred bdd_ithvar(in(_), go(BDD)) :: 
                  c_int * c_int + (foreign, returns(BDD)).

:- true pred bdd_nithvar(in(_), go(BDD)) :: 
                  c_int * c_int + (foreign, returns(BDD)).

:- true pred bdd_apply(in(_), in(_), in(_), go(BDD)) :: 
                  c_int * c_int * c_int * c_int + (foreign, returns(BDD)).

:- true pred bdd_addref(in(_), go(BDD)) :: 
                  c_int * c_int + (foreign, returns(BDD)).

:- true pred bdd_delref(in(_), go(BDD)) :: 
                  c_int * c_int + (foreign, returns(BDD)).
                  
:- true pred bdd_makeset(in(_), in(_), go(BDD)) :: 
                  c_int * c_int * c_int + (foreign, returns(BDD)).
                
:- true pred bdd_exists(in(_), in(_), go(BDD)) :: 
                  c_int * c_int * c_int + (foreign, returns(BDD)).
                  
:- true pred bdd_newpair(go(Pair)) ::
				  address + (foreign, returns(Pair)).
                  
:- true pred bdd_setpair(in(_), in(_), in(_), go(R)) ::
				  address * c_int * c_int * c_int + (foreign, returns(R)).
				  
:- true pred bdd_replace(in(_), in(_), go(R)) ::
				  c_int * address * c_int + (foreign, returns(R)).
				  
:- true pred bdd_false(go(BDD)) ::
				  c_int + (foreign, returns(BDD)).

:- true pred bdd_true(go(BDD)) ::
				  c_int + (foreign, returns(BDD)).
				  
:- true pred bdd_not(in(_),go(BDD)) ::
				  c_int * c_int + (foreign, returns(BDD)).
				  
:- true pred bdd_ite(in(_), in(_), in(_), go(BDD)) :: 
                  c_int * c_int * c_int * c_int + (foreign, returns(BDD)).
                  
:- true pred bdd_high(in(_), go(BDD)) ::
				  c_int * c_int + (foreign, returns(BDD)).

:- true pred bdd_low(in(_), go(BDD)) ::
				  c_int * c_int + (foreign, returns(BDD)).
              
:- true pred bdd_and(in(_), in(_), go(BDD)) :: 
                  c_int * c_int * c_int + (foreign, returns(BDD)).

:- true pred bdd_or(in(_), in(_), go(BDD)) :: 
                  c_int * c_int * c_int + (foreign, returns(BDD)).
               
:- true pred bdd_printtable(in(_)) :: 
                  c_int + foreign.

:- include(.(ciao_buddy_config_auto)).

:- use_foreign_library(bdd).
