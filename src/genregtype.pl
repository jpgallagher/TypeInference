% Type inference system (designed by M. Bruynooghe and J. Gallagher)
% Version 1.3 created by jpg on 08/08/2006
% (c) Roskilde University
 
:- module(genregtype, [
	genregtype/2,
	writeRegType/2]).

genregtype(Ts,Rs) :-
	genTransitions(Ts,Rs,[]).
	
genTransitions([],Rs,Rs).
genTransitions([typedef(T,Cs)|Ts],Rs0,Rs2) :-
	genRegRules(Cs,T,Rs0,Rs1),
	genTransitions(Ts,Rs1,Rs2).
	
genRegRules([],_,Rs,Rs).
genRegRules([C|Cs],T,[(C1->T1)|Rs0],Rs1) :-
	removeparams((C -> T), (C1 -> T1)),
	genRegRules(Cs,T,Rs0,Rs1).
	
removeparams((C -> T), (C1 -> T1)) :-
	C =.. [F|Xs],
	stripParams(Xs,Ys),
	C1 =.. [F|Ys],
	T =.. [T1|_].
	
stripParams([],[]).
stripParams(['$VAR'(_)|Xs],[dynamic|Ys]) :-
	!,
	stripParams(Xs,Ys).
stripParams([X|Xs],[F|Ys]) :-
	X =.. [F|_],
	stripParams(Xs,Ys).
	
writeRegType(Ts,F) :-
	open(F,write,S),
	writeTransitions(Ts,S),
	close(S).
	
writeTransitions([],_).
writeTransitions([(L->R)|Ts],S) :-
	writeq(S,(L -> R)),
	write(S,'.'),
	nl(S),
	writeTransitions(Ts,S).
	
