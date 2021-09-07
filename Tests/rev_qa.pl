rev_query(A,B) :-
    rev_query([C|A],D).
app_query(A,[B],C) :-
    rev_query([B|D],C),rev_ans(D,A).
app_query(A,B,C) :-
    app_query([D|A],B,[D|C]).
rev_ans([A|B],C) :-
    rev_query([A|B],C),rev_ans(B,D),app_ans(D,[A],C).
rev_ans([],[]) :-
    rev_query([],[]).
app_ans([],A,A) :-
    app_query([],A,A).
app_ans([A|B],C,[A|D]) :-
    app_query([A|B],C,[A|D]),app_ans(B,C,D).
rev_query(A,B) :-
    true.
