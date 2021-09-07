# TypeInference

Type inference system (designed by M. Bruynooghe and J. Gallagher)
(c) Roskilde University
 
Type inference tool for Prolog
==============================
Derives a polymorphic well-typing for a given logic program.

Runs in Ciao-Prolog 

Requirements
============

Library chclibs (https://github.com/bishoksan/chclibs)

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

References
==========
Maurice Bruynooghe, John P. Gallagher, Wouter Van Humbeeck:
Inference of Well-Typings for Logic Programs with Application to Termination Analysis. SAS 2005: 35-51
DOI 10.1007/978-3-540-27775-0_3

@inproceedings{DBLP:conf/iclp/GallagherH04,
  author    = {John P. Gallagher and
               Kim S. Henriksen},
  editor    = {Bart Demoen and
               Vladimir Lifschitz},
  title     = {Abstract Domains Based on Regular Types},
  booktitle = {Logic Programming, 20th International Conference, {ICLP} 2004, Saint-Malo,
               France, September 6-10, 2004, Proceedings},
  series    = {Lecture Notes in Computer Science},
  volume    = {3132},
  pages     = {27--42},
  publisher = {Springer},
  year      = {2004},
  url       = {https://doi.org/10.1007/978-3-540-27775-0\_3},
  doi       = {10.1007/978-3-540-27775-0\_3},
  timestamp = {Tue, 14 May 2019 10:00:48 +0200},
  biburl    = {https://dblp.org/rec/conf/iclp/GallagherH04.bib},
  bibsource = {dblp computer science bibliography, https://dblp.org}
}


(c) Roskilde University,  2004 - 2021

Algorithm designed by Maurice Bruynooghe and John Gallagher
Contact JG (jpg@ruc.dk) to report bugs or suggest improvements.
