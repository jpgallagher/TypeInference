% Type inference system (designed by M. Bruynooghe and J. Gallagher)
% Version 1.3 created by jpg on 08/08/2006
% (c) Roskilde University
 
:- module(solveconstraints, [
	solveconstraints/6]).


:- use_module(library(lists)).
:- use_module(chclibs(timer_ciao)).
:- use_module(chclibs(balanced_tree)).

solveconstraints(Cs,TDefs,Sigs0,Sigs2,V0,V2) :-
	V1 is V0-1,
	initEqClasses(V1,root,Es0),
	addConstraints(Cs,Es0,Es1,V0,V2),
	end_time('Time to solve: ', user_output),
	traverse_tree(Es1,AllTypes),
	allTypeDefs(AllTypes,Es1,Ts),
	substClasses(Sigs0,Sigs1,Es1),
	parameterise(Ts,TDefs,Sigs1,Sigs2),
	end_time('Time to parametrise: ', user_output).
	
initEqClasses(V,Es0,Es0) :-
	V < 0.
initEqClasses(V0,Es0,Es2) :-
	V0>=0,
	V1 is V0-1,
	make(V0,Es0,Es1),
	!,
	initEqClasses(V1,Es1,Es2).

% The main constraint solving loop

addConstraints([C|Cs],Es0,Es2,V0,V2) :-
	addConstraint(C,Es0,Es1,V0,V1,NewCs,Cs),
	!,
	addConstraints(NewCs,Es1,Es2,V1,V2).
addConstraints([],Es0,Es0,V0,V0).

addConstraint('$VAR'(T1)='$VAR'(T2),Es0,Es3,V0,V1,NewCs,Cs) :-
	merge(T1,T2,Es0,Es1,data(R,Ts)),
	normaliseData(Ts,Ts1,NewCs,Cs,Es1,Es2,V0,V1),
	search_replace_tree(Es2,R,_,Es3,data(R,Ts1)).
addConstraint('$VAR'(T1)>=F,Es0,Es3,V0,V1,NewCs,Cs) :-
	find(0,T1,Es0,Es1,data(R,Ts)),
	addAndNormalise(Ts,F,Ts2,NewCs,Cs,Es1,Es2,V0,V1),
	search_replace_tree(Es2,R,_,Es3,data(R,Ts2)).
	
normaliseData([F1,F2|Fs],[F3|Fs1],NewCs0,NewCs2,Es0,Es2,V0,V2) :-
	functor(F1,F,N),
	functor(F2,F,N),
	!,
	makeNewCase(F1,F2,F3,NewCs0,NewCs1,Es0,Es1,V0,V1),
	normaliseData(Fs,Fs1,NewCs1,NewCs2,Es1,Es2,V1,V2).
normaliseData([F1|Fs],[F1|Fs1],NewCs0,NewCs1,Es0,Es1,V0,V1) :-
	normaliseData(Fs,Fs1,NewCs0,NewCs1,Es0,Es1,V0,V1).
normaliseData([],[],NewCs,NewCs,Es0,Es0,V0,V0).
	
makeNewCase(F1,F2,F,NewCs0,NewCs1,Es0,Es1,V0,V1) :-
	F1 =.. [P|Xs],
	F2 =.. [P|Ys],
	compareArgs(Xs,Ys,Zs,NewCs0,NewCs1,Es0,Es1,V0,V1),
	F =.. [P|Zs].

compareArgs([],[],[],NewCs,NewCs,Es0,Es0,V,V).
compareArgs([X|Xs],[Y|Ys],[X|Zs],[X>=Y|NewCs0],NewCs1,Es0,Es1,V0,V1) :-
	varType(X),
	nonTypeVar(Y),
	compareArgs(Xs,Ys,Zs,NewCs0,NewCs1,Es0,Es1,V0,V1).
compareArgs([X|Xs],[Y|Ys],[Y|Zs],[Y>=X|NewCs0],NewCs1,Es0,Es1,V0,V1) :-
	nonTypeVar(X),
	varType(Y),
	compareArgs(Xs,Ys,Zs,NewCs0,NewCs1,Es0,Es1,V0,V1).
compareArgs([X|Xs],[Y|Ys],[X|Zs],[X=Y|NewCs0],NewCs1,Es0,Es1,V0,V1) :-
	varType(X),
	varType(Y),
	compareArgs(Xs,Ys,Zs,NewCs0,NewCs1,Es0,Es1,V0,V1).
compareArgs([X|Xs],[Y|Ys],[NewT|Zs],[NewT>=X, NewT>=Y|NewCs0],NewCs1,Es0,Es2,V0,V2) :-
	nonTypeVar(X),
	nonTypeVar(Y),
	makeNewType(V0,NewT,V1),
	make(V0,Es0,Es1),
	compareArgs(Xs,Ys,Zs,NewCs0,NewCs1,Es1,Es2,V1,V2).
	
addAndNormalise([],F,[F],Cs,Cs,Es0,Es0,V0,V0).
addAndNormalise([F|Ts],F,[F|Ts],NewCs0,NewCs0,Es0,Es0,V0,V0) :-
	!.
addAndNormalise([F1|Ts],F,[F2|Ts],NewCs0,NewCs1,Es0,Es1,V0,V1) :-
	functor(F1,P,N),	
	functor(F,P,N),	
	!,
	makeNewCase(F,F1,F2,NewCs0,NewCs1,Es0,Es1,V0,V1).
addAndNormalise([F1|Ts],F,[F1|Ts1],NewCs0,NewCs1,Es0,Es1,V0,V1) :-
	F1 @< F,
	!,
	addAndNormalise(Ts,F,Ts1,NewCs0,NewCs1,Es0,Es1,V0,V1).
addAndNormalise([F1|Ts],F,[F,F1|Ts],NewCs0,NewCs0,Es0,Es0,V0,V0).
	 

makeNewType(J,'$VAR'(J),J1) :-
	J1 is J+1.
	

nonTypeVar(X) :-
	\+ varType(X).

varType('$VAR'(X)) :-
	number(X).

allTypeDefs([],_,[]).
allTypeDefs([rec(T,data(T,Fs))|Ts],Es,[typedef('$VAR'(T),Fs1)|Ts1]) :-
	!,
	substClasses(Fs,Fs1,Es),
	allTypeDefs(Ts,Es,Ts1).
allTypeDefs([_|Ts],Es,Ts1) :-
	allTypeDefs(Ts,Es,Ts1).

substClasses(X,'$VAR'(M),Es0) :- % members of same class identified
	varType(X),
	X='$VAR'(N),
	findSimple(0,N,Es0,data(M,_)), 	
	!.
substClasses(Y,Y,_) :-	
	varType(Y),
	!.
substClasses(F,F1,Es) :-
	F =.. [P|Xs],
	substEachClasses(Xs,Xs1,Es),
	F1 =.. [P|Xs1].
	
substEachClasses([],[],_).
substEachClasses([X|Xs],[X1|Xs1],Es) :-
	substClasses(X,X1,Es),
	substEachClasses(Xs,Xs1,Es).
	
	
	
% Find the type parameters and reformulate type definitions.

parameterise(TList,TDefs1,Sigs0,Sigs1) :-
	rebuildDefTree(TList,root,Ts0),
	digraph(TList,root,Ds),
	findParams(TList,Ts0,Ds,root,PolyTypes),
	substPolyTypeDefs(TList,TDefs1,PolyTypes),
	substPolyTypeDefs(Sigs0,Sigs1,PolyTypes).
	
rebuildDefTree([],Ts0,Ts0).
rebuildDefTree([typedef(T,Fs)|TList],Ts0,Ts2) :-
	insert_tree(Ts0,T,Fs,Ts1),
	rebuildDefTree(TList,Ts1,Ts2).

digraph([typedef(T,Cs)|Ts],Ds0,Ds2) :-
	typeSubterms(Cs,[],Ss),
	insertLinks(Ss,T,Ds0,Ds1),
	digraph(Ts,Ds1,Ds2).
digraph([],Ds,Ds).

typeSubterms(T,S,S1) :-
	varType(T),
	!,
	insertIfNew(T,S,S1).
typeSubterms(F,S0,S1) :-
	F =.. [_|Xs],
	typeSubtermsList(Xs,S0,S1).
	
insertIfNew(T,Cs,Cs) :-
	member(T,Cs),
	!.
insertIfNew(T,Cs,[T|Cs]).

typeSubtermsList([],S,S).
typeSubtermsList([X|Xs],S0,S2) :-
	typeSubterms(X,S0,S1),
	typeSubtermsList(Xs,S1,S2).
	
insertLinks([],_,Ds,Ds).
insertLinks([C|Cs],T,Ds0,Ds2) :-
	insertLink(Ds0,T,C,Ds1),
	insertLinks(Cs,T,Ds1,Ds2).
	
insertLink(Ds0,X,T,Ds1) :-
	search_replace_tree(Ds0,X,Ls,Ds1,Ls1),
	!,
	insertIfNew(T,Ls,Ls1).
insertLink(Ds0,X,T,Ds1) :-
	insert_tree(Ds0,X,[T],Ds1).
	
findParams([typedef(T,[])|Ts],TDefs,Ds,PTypes0,PTypes2) :-
	insert_tree(PTypes0,T,T,PTypes1),
	findParams(Ts,TDefs,Ds,PTypes1,PTypes2).
findParams([typedef(T,[_|_])|Ts],TDefs,Ds,PTypes0,PTypes2) :-
	reachable([T],Ds,[],As1),
	undefinedTypes(As1,TDefs,As),
	arg(1,T,K),
	newShortName(K,TK),
	P =.. [TK|As],
	insert_tree(PTypes0,T,P,PTypes1),
	findParams(Ts,TDefs,Ds,PTypes1,PTypes2).
findParams([],_,_,Ps,Ps).

	
newShortName(K,TK) :-
	name(K,KN),
	append("t",KN,TKN),
	name(TK,TKN).
	
reachable([T|Ts],Ds,Rs0,Rs1) :-
	member(T,Rs0),
	!,
	reachable(Ts,Ds,Rs0,Rs1).
reachable([T|Ts],Ds,Rs0,Rs1) :-
	links(Ds,T,Vs),
	append(Vs,Ts,Ts1),
	reachable(Ts1,Ds,[T|Rs0],Rs1).
reachable([],_,Rs,Rs).

links(Ds,T,Vs) :-
	search_tree(Ds,T,Vs),
	!.
links(_,_,[]).
	
undefinedTypes([A|As],DefinedTypes, Us) :-
	definedType(A,DefinedTypes),
	!,
	undefinedTypes(As,DefinedTypes, Us).
undefinedTypes([A|As],DefinedTypes, [A|Us]) :-
	undefinedTypes(As,DefinedTypes, Us).
undefinedTypes([],_,[]).

definedType(A,Ts) :-
	search_tree(Ts,A,[_|_]).

substitutePolyType(X,P,Es0) :- 
	varType(X),
	search_tree(Es0,X,P), 	
	!.
substitutePolyType(F,F1,Es) :-
	F =.. [P|Xs],
	substPolyTypeDefs(Xs,Xs1,Es),
	F1 =.. [P|Xs1].
	
substPolyTypeDefs([X|Xs],[X1|Xs1],PolyTypes) :-
	substitutePolyType(X,X1,PolyTypes),
	substPolyTypeDefs(Xs,Xs1,PolyTypes).
substPolyTypeDefs([],[],_).

renameTypes([],_,[]).
renameTypes([X|Xs],Ns0,[Z|Zs]) :-
	search_tree(Ns0,X,Z),
	renameTypes(Xs,Ns0,Zs).
	
% union-find operations

find(_N,T,Es0,Es2,R) :-
	search_replace_tree(Es0,T,PT,Es1,R1),
	%N1 is N+1,
	checkRoot(_N1,PT,T,R,R1,Es1,Es2).

findSimple(_N,T,Es0,R) :-
	search_tree(Es0,T,PT),
	%N1 is N+1,
	checkRootSimple(_N1,PT,T,R,Es0).
	
checkRoot(_N1,data(T,Fs),T,data(T,Fs),data(T,Fs),Es,Es) :-
	%write(N1), write('steps for find '), nl,
	!.
checkRoot(_N,data(PT,_),_,data(R,Fs),data(R,[]),Es0,Es1) :-
	% N1 is N+1,
	find(_N1,PT,Es0,Es1,data(R,Fs)).
	
checkRootSimple(_N1,data(T,Fs),T,data(T,Fs),_) :-
	%write(N1), write('steps for simple find '), nl,
	!.
checkRootSimple(_N,data(PT,_),_,R,Es0) :-
	% N1 is N+1,
	findSimple(_N1,PT,Es0,R).
	
make(X,Es0,Es1) :-
	insert_tree(Es0,X,data(X,[]),Es1).
	
merge(X,Y,Es0,Es4,data(RX,Ts)) :-
	find(0,X,Es0,Es1,data(RX,TsX)),
	find(0,Y,Es1,Es2,data(RY,TsY)),
	!,
	mergeData(TsX,TsY,Ts),
	search_replace_tree(Es2,RY,_,Es3,data(RX,[])),
	search_replace_tree(Es3,RX,_,Es4,data(RX,Ts)).

mergeData([],Ts,Ts).
mergeData([T|Ts],[],[T|Ts]).
mergeData([T1|Ts1],[T2|Ts2],[T3|Ts3]) :-
	compareTerms(T1,T2,T3,Ts1,Ts2,Ss1,Ss2),
	mergeData(Ss1,Ss2,Ts3).

compareTerms(T,T,T,Ts1,Ts2,Ts1,Ts2).
compareTerms(T1,T2,T1,Ts1,Ts2,Ts1,[T2|Ts2]) :-
	T1 @< T2.
compareTerms(T1,T2,T2,Ts1,Ts2,[T1|Ts1],Ts2) :-
	T2 @< T1.


	
link(X,X,Es,Es) :-
	!.
link(RX,RY,Es0,Es1) :-
	search_replace_tree(Es0,RY,_,Es1,RX). 

