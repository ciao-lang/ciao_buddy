:- module(bdd_build_test,_).

:- use_module(ciao_buddy(ciao_buddy)).
	
test(E) :-
	bdd_init(1000,100,_),
	bdd_setvarnum(20,_),
	bdd_build(E,B),
	bdd_printtable(B),
	bdd_done(0).
  
main :-
	test(a*(b+c)),
	test('$VAR'(0)*('$VAR'(1)+'$VAR'(2))),
	test((p->(q->p))),
	test((x=1)*(x=0)).