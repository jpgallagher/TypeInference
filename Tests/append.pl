
app([],Ys,Ys).
app([X|Xs],Ys,[X|Zs]) :-
	app(Xs,Ys,Zs).

%p(X,L) :- app([X],[[X]],L).
%p(L,R) :- app(L,[L],R).
