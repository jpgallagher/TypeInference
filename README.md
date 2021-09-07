# TypeInference

Type inference system (designed by M. Bruynooghe and J. Gallagher)
(c) Roskilde University
 
Type inference tool
===================
Derives a polymorphic well-typing for a given program.

Runs in SICStus Prolog, Ciao-Prolog and SWI-Prolog (others not tried yet).

Usage
=====

% prolog

?- use_module(types).

?- types(+Filename).

To run a number of tests, type

?- test.

Ciao Command line version
=========================
A command line version is produced by compiling using the Ciao compiler

% ciaoc types.pl

Then run the analyser as follows.

% types [options] myfile.pl

where options are

-o filename 			output file for type definitions (default is user output)
-regtype filename 		output file for output as regular types (default is no output)


(c) Roskilde University,  2004 - 2021

Algorithm designed by Maurice Bruynooghe and John Gallagher
Contact JG (jpg@ruc.dk) to report bugs or suggest improvements.