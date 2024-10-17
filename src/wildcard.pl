:- module(wildcard, [patt//1]).
:- use_module(library(dcgs)).

% match a list to pattern list.
patt([])     --> [].
patt([C|Cs]) -->
  wildcard([C|Cs]) | placeholder([C|Cs]) | escape([C|Cs]) | regular([C|Cs]).

wildcard(['*'|Cs]) --> word([]), patt(Cs).

placeholder(['?'|Cs]) --> [_], patt(Cs).

escape(['\\'|Cs]) --> same(Cs).

regular([C|Cs]) --> [C], patt(Cs).

% matches any two identical list heads.
same([])     --> [].
same([C|Cs]) --> [C], patt(Cs).

% matches any list with an empty list.
word([]) --> [].
word([]) --> [_], word([]).
