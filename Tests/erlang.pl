/* Erlang function from paper by Marlow and Wadler

-deftype tree(A,B) =
T when T = empty | branch{A,B,T,T}.
-type new() -> tree(0,0).
new() -> empty.
-type insert(A,B,tree(A,B)) -> tree(A,B).
insert(K0,V0,empty) ->
{branch,K0,V0,empty,empty};
insert(K0,V0,{branch,K,V,L,R}) ->
if K0 < K ->
{branch,K,V,insert(K0,V0,L),R};
K0 == K ->
{branch,K0,V0,L,R};
true ->
{branch,K,V,L,insert(K0,V0,R)}
end.
-type lookup(A,tree(A,B)) -> B | error
when B \ error.
lookup(K0,empty) -> error;
lookup(K0,{branch,K,V,L,R}) ->
if K0 < K -> lookup(K0,L);
K0 == K -> V;
true -> lookup(K0,R)

*/

new(empty).

insert(K0,V0, empty, branch(K0,V0,empty,empty)).
insert(K0,V0, branch(K,V,L,R), branch(K,V,L1,R)) :-
	K0 < K,
	insert(K0,V0,L,L1).
insert(K0,V0, branch(K,V,L,R), branch(K,V,L,R)) :-
	K0 == K.
insert(K0,V0, branch(K,V,L,R), branch(K,V,L,R1)) :-
	K0 >= K,
	insert(K0,V0,R,R1).
	
lookup(K0,empty,error).
lookup(K0, branch(K,V,L,R), V0) :-
	K0 < K,
	lookup(K0, L, V0).
lookup(K0, branch(K,V,L,R), V) :-
	K0 == K.
lookup(K0, branch(K,V,L,R), V0) :-
	K0 >= K,
	lookup(K0, R, V0).
