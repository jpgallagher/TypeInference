% Type inference system (designed by M. Bruynooghe and J. Gallagher)
% (c) Roskilde University
 
:- module(types, [types/1, types/3, test/0, main/1]).


:- use_module(library(lists)).
:- use_module(genconstraints).
:- use_module(chclibs(balanced_tree)).
:- use_module(solveconstraints).
:- use_module(genregtype).
:- use_module(chclibs(timer_ciao)).

:- dynamic verbose/0.
	
main(ArgV) :-
	get_options(ArgV,Options,[F|_]),
	(member(verbose,Options) -> start_time; true),
	types(F,TDefs,Signatures),
	outputFileStream(Options,Stream),
	writeTypes(Stream,TDefs,Signatures),
	close(Stream),
	(member(verbose,Options) -> end_time('Time: ',user_output); true),
	writeRegtypefile(TDefs,Options).
	
% optionally write out types as a regular type
writeRegtypefile(TDefs,Options) :-
	member(regtypefile(RFile),Options),
	!,
	genregtype(TDefs,Rs),
	writeRegType(Rs,RFile).
writeRegtypefile(_,_).
	
% get_options/3 provided by Michael Leuschel
get_options([],[],[]).
get_options([X|T],Options,Args) :-
   (recognised_option(X,Opt,Values) ->
       ( append(Values, Rest, T),
	 RT = Rest,
	 Options = [Opt|OT], Args = AT
       )
   ;
       (
	 Options = OT,     Args = [X|AT],
	 RT = T
       )
   ),
   get_options(RT,OT,AT).


recognised_option('-o',output_file(OF),[OF]).
recognised_option('-regtype',regtypefile(R),[R]).
recognised_option('-v',verbose,[]).


outputFileStream(Options,S) :-
	member(output_file(F),Options),
	!,
	open(F,write,S).
outputFileStream(_,user_output).	


types(F,TDefs,Signatures) :-
	genconstraints(F,Cs,Sigs0,V),
	solveconstraints(Cs,TDefs,Sigs0,Signatures,V,_).
	
types(F) :-
	main([F]).
	
writeTypes(Stream,TDefs,Signatures) :-
	write(Stream, 'Type definitions'), 
	nl(Stream),
	nl(Stream),
	writeTypeDefs(TDefs,Stream),
	nl(Stream),
	write(Stream, 'Signatures'), nl(Stream),
	writeList(Signatures,Stream),
	nl(Stream),
	nl(Stream).
	
	
%--------------
	
writeTypeDefs([],_).
writeTypeDefs([typedef(T,Cs)|Ts],S) :-
	write(S,T),
	write(S,' --> '),
	writeCases(Cs,S),
	nl(S),
	writeTypeDefs(Ts,S).
	
writeCases([],_).
writeCases([C],S) :-
	write(S,C).
writeCases([C1,C2|Cs],S) :-
	write(S,C1),
	write(S,'; '),
	writeCases([C2|Cs],S).
	
writeList([],S) :-
	nl(S).
writeList([C|Cs],S) :-
	write(S,C),
	nl(S),
	writeList(Cs,S).

%---tests-------------

testtypes(F) :-
	main([F]).
	
test :-
	write('==================='),nl,
	write('Tests/append.pl'),nl,
	write('==================='),nl,
	testtypes('Tests/append.pl'),
	
	write('==================='),nl,
	write('Tests/rev.pl'),nl,
	write('==================='),nl,
	testtypes('Tests/rev.pl'),
	
	write('==================='),nl,
	write('Tests/trans.pl'),nl,
	write('==================='),nl,
	testtypes('Tests/trans.pl'),
	
	write('==================='),nl,
	write('Tests/frev.pl'),nl,
	write('==================='),nl,
	testtypes('Tests/frev.pl'),
	
	write('==================='),nl,
	write('Tests/pq.pl'),nl,
	write('==================='),nl,
	testtypes('Tests/pq.pl'),
	
	write('==================='),nl,
	write('Example from Combined Norms paper'),nl,
	write('==================='),nl,
	testtypes('Tests/combinednorm_p.pl'),
	
	write('==================='),nl,
	write('Transpose from Combined Norms paper'),nl,
	write('==================='),nl,
	testtypes('Tests/combinednorm_trans.pl'),
	
	write('==================='),nl,
	write('parse from Combined Norms paper'),nl,
	write('==================='),nl,
	testtypes('Tests/parse.pl'),
	
	write('==================='),nl,
	write('Ackermann'),nl,
	write('==================='),nl,
	testtypes('Tests/combinednorm_ack.pl'),
	
	write('==================='),nl,
	write('Tests/dnf.pl'),nl,
	write('==================='),nl,
	testtypes('Tests/dnf.pl'),
	
	write('==================='),nl,
	write('Tests/qsort.pl'),nl,
	write('==================='),nl,
	testtypes('Tests/qsort.pl').
	
