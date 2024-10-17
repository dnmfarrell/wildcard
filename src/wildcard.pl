:- module(wildcard, [patt//1]).
:- use_module(library(dcgs)).

% match a list to pattern list.
patt([],A,B) :-
  A=B.
patt([X|A],B,C) :-
  (  wildcard(X) ->
     word([],B,D),
     patt(A,D,C)
  ;  placeholder(X) ->
     B=[D|E],
     patt(A,E,C)
  ;  escape(X) ->
     same(A,B,C)
  ;  B=[X|D],
     D=E,
     patt(A,E,C)
  ).

% matches any two identical list heads.
same([])     --> [].
same([C|Cs]) --> [C], patt(Cs).

% matches any list with an empty list.
word([]) --> [].
word([]) --> [_], word([]).

wildcard('*').
placeholder('?').
escape('\\').
