# TypeInference

Type inference system (designed by M. Bruynooghe and J. Gallagher)
(c) Roskilde University
 
## Type inference tool for Prolog

Derives a polymorphic well-typing for a given logic program.

Runs in Ciao-Prolog 

## Requirements

Library chclibs (https://github.com/bishoksan/chclibs)

## Usage


```
% prolog

?- use_module(types).
?- types(+Filename,TDefs,Signatures)
```

To run a number of tests, type

```
?- test.
```

## Ciao Command line version

A command line version is produced by compiling using the Ciao compiler

% ciaoc types.pl

Then run the analyser as follows.

% types [options] myfile.pl

where options are

```
-o filename 			output file for type definitions (default is user output)
-regtype filename 		output file for output as regular types (default is no output)
```

## References

Algorithm description.

Maurice Bruynooghe, John P. Gallagher, Wouter Van Humbeeck:
Inference of Well-Typings for Logic Programs with Application to Termination Analysis. SAS 2005: 35-51.
DOI 10.1007/978-3-540-27775-0_3

```
@inproceedings{DBLP:conf/sas/BruynoogheGH05,
  author    = {Maurice Bruynooghe and
               John P. Gallagher and
               Wouter Van Humbeeck},
  editor    = {Chris Hankin and
               Igor Siveroni},
  title     = {Inference of Well-Typings for Logic Programs with Application to Termination
               Analysis},
  booktitle = {Static Analysis, 12th International Symposium, {SAS} 2005, London,
               UK, September 7-9, 2005, Proceedings},
  series    = {Lecture Notes in Computer Science},
  volume    = {3672},
  pages     = {35--51},
  publisher = {Springer},
  year      = {2005},
  url       = {https://doi.org/10.1007/11547662\_5},
  doi       = {10.1007/11547662\_5},
  timestamp = {Tue, 14 May 2019 10:00:52 +0200},
  biburl    = {https://dblp.org/rec/conf/sas/BruynoogheGH05.bib},
  bibsource = {dblp computer science bibliography, https://dblp.org}
}
```

(c) Roskilde University,  2004 - 2021


Contact JG (jpg@ruc.dk) to report bugs or suggest improvements.
