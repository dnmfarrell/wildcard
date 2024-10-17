:- module(wildcard, [patt//1]).
:- use_module(library(between)).
:- use_module(library(dcgs)).
:- use_module(library(lists), [memberchk/2]).

% match a list to list of patterns containing wildcards * and ?.
patt([])        --> [].
patt(['*'|Cs])  --> word([]), patt(Cs).
patt(['?'|Cs])  --> [C], patt(Cs).
patt(['\\'|Cs]) --> same(Cs).
patt([C|Cs])    --> [C], { reg_char(C) }, patt(Cs).

% matches any two identical list heads.
same([])     --> [].
same([C|Cs]) --> [C], patt(Cs).

% matches any list with an empty list.
word([]) --> [].
word([]) --> [C], word([]).

reg_char(Char) :-
  (  nonvar(Char),
     memberchk(Char, "*?\\") ->
     fail
  ;  true
  ).
