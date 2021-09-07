% Type inference system (designed by M. Bruynooghe and J. Gallagher)
% Version 1.3 created by jpg on 08/08/2006
% (c) Roskilde University
 
:- module(genconstraints, [
	genconstraints/4]).

:- use_module(chclibs(readprog)).
:- use_module(library(lists)).


genconstraints(F,Cs,Signatures,V) :-
	readprog(F,Cls),
	genConstraints(Cls,Cs,0,V,Signatures). 	% initialise clause and variable counters
	
genConstraints([predicates(Ps)|Cls],Cs0,V0,V2,Signatures) :-
	makeSignatures(Ps,Signatures,V0,V1),
	genConstraints1(Cls,Signatures,Cs0,[],V1,V2).
genConstraints1([clause(Cl,_)|Cls],Sigs,Cs0,Cs3,V0,V4) :-
	renameVars(Cl,(H :- B),V0,V1),
	genAtomConstraints(H,Sigs,Cs0,Cs1,V1,V2),
	genBodyConstraints(B,Sigs,Cs1,Cs2,V2,V3),
	genConstraints1(Cls,Sigs,Cs2,Cs3,V3,V4).
genConstraints1([],_,Cs,Cs,V,V).

genAtomConstraints(X=Y,_,Cs0,Cs1,V0,V1) :-	%  constraints on =/2 args
	!,
	genEqConstraints(X,Y,Cs0,Cs1,V0,V1).
genAtomConstraints(A,Sigs,Cs0,Cs1,V0,V0) :-
	functor(A,P,N),
	functor(Sig,P,N),
	member(Sig,Sigs),
	genArgConstraints(1,N,A,Sig,Cs0,Cs1).
	
genEqConstraints(X,Y,[X=Y|Cs0],Cs0,V0,V0) :-
	varType(X),
	varType(Y),
	!.
genEqConstraints(X,Y,[X>=Y|Cs0],Cs0,V0,V0) :-
	varType(X),
	!.
genEqConstraints(X,Y,[Y>=X|Cs0],Cs0,V0,V0) :-
	varType(Y),
	!.
genEqConstraints(X,Y,[Z>=X, Z>=Y|Cs0],Cs0,V0,V1) :-
	newVar(V0,Z),
	V1 is V0+1.
	
newVar(V0,'$VAR'(V0)).

	
genBodyConstraints(true,_,Cs,Cs,V0,V0) :-
	!.
genBodyConstraints((B,Bs),Sigs,Cs0,Cs2,V0,V2) :-
	!,
	genBodyConstraints(B,Sigs,Cs0,Cs1,V0,V1),
	genBodyConstraints(Bs,Sigs,Cs1,Cs2,V1,V2). 
genBodyConstraints((B;Bs),Sigs,Cs0,Cs2,V0,V2) :-
	!,
	genBodyConstraints(B,Sigs,Cs0,Cs1,V0,V1),
	genBodyConstraints(Bs,Sigs,Cs1,Cs2,V1,V2). 
genBodyConstraints((B->Bs),Sigs,Cs0,Cs2,V0,V2) :-
	!,
	genBodyConstraints(B,Sigs,Cs0,Cs1,V0,V1),
	genBodyConstraints(Bs,Sigs,Cs1,Cs2,V1,V2). 
genBodyConstraints((\+ B),Sigs,Cs0,Cs1,V0,V1) :-
	!,
	genBodyConstraints(B,Sigs,Cs0,Cs1,V0,V1). 
genBodyConstraints(B,Sigs,Cs0,Cs1,V0,V1) :-
	genAtomConstraints(B,Sigs,Cs0,Cs1,V0,V1). 
	
	
genArgConstraints(J,N,_,_,Cs,Cs) :-
	J > N.
genArgConstraints(J,N,A,Sig,[C|Cs0],Cs1) :-
	J =< N,
	J1 is J+1,
	genArgConstraint(A,J,Sig,C),
	genArgConstraints(J1,N,A,Sig,Cs0,Cs1).
	
genArgConstraint(A,J,Sig,T=X) :-
	arg(J,A,X),
	varType(X),
	!,
	arg(J,Sig,T).
genArgConstraint(A,J,Sig,T >= F ) :-
	arg(J,A,F),
	arg(J,Sig,T).
	
makeSignatures([],[],V0,V0).
makeSignatures([P/N|Ps],[PSig|Signatures],V0,V2) :-
	functor(PSig,P,N),
	argTypes(1,N,P/N,PSig,V0,V1),
	makeSignatures(Ps,Signatures,V1,V2).
	
argTypes(J,N,_,_,V,V) :-
	J > N.
argTypes(J,N,P/N,PSig,V0,V2) :-
	J =< N,
	J1 is J+1,
	arg(J,PSig,'$VAR'(V0)),
	V1 is V0+1,
	argTypes(J1,N,P/N,PSig,V1,V2).

varType('$VAR'(X)) :-
	number(X).
	
renameVars(T1,T2,Base,V) :-
	renamevars1(T1,T2,Base,Base,V).
	
renamevars1('$VAR'(N),'$VAR'(M),V0,V1,V2) :-
	number(N),
	!,
	M is N+V0,
	M1 is M+1,
	max(V1,M1,V2).
renamevars1(T1,T2,Base,CurrMax,Max) :-
	T1 =.. [F|Xs],
	renameVarList(Xs,Ys,Base,CurrMax,Max),
	T2 =.. [F|Ys].
	
max(X,Y,X) :-
	X>Y,
	!.
max(_,Y,Y).
	
renameVarList([],[],_,Max,Max).
renameVarList([X|Xs],[Y|Ys],Base,CurrMax,Max) :-
	renamevars1(X,Y,Base,CurrMax,NewMax),
	renameVarList(Xs,Ys,Base,NewMax,Max).
	

	
