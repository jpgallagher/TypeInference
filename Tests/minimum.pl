minimum(tree(X, void, Y), X).
minimum(tree(_, Left, Y), Z) :- minimum(Left, Z).

p(S,M) :- minimum(tree(a,S,S),M).

