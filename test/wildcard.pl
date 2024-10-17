:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module('../src/wildcard', [patt//1]).

test("a-ab",    (\+ phrase(patt("a"), "ab"))).
test("A-a",     (\+ phrase(patt("A"), "a"))).
test("aB-ab",   (\+ phrase(patt("aB"), "ab"))).
test("abC-abc", (\+ phrase(patt("abC"), "abc"))).
test("a-a",     (phrase(patt("a"), "a"))).
test("ab-ab",   (phrase(patt("ab"), "ab"))).

test("?a-a",    (\+ phrase(patt("?a"), "a"))).
test("a?-a",    (\+ phrase(patt("a?"), "a"))).
test("?-a",     (phrase(patt("?"), "a"))).
test("a?-ab",   (phrase(patt("a?"), "ab"))).
test("a?c-abc", (phrase(patt("a?c"), "abc"))).
test("???-abc", (phrase(patt("???"), "abc"))).

test("*a-a",    (phrase(patt("*a"), "a"))).
test("a*-a",    (phrase(patt("a*"), "a"))).
test("*-a",     (phrase(patt("*"), "a"))).
test("a*-ab",   (phrase(patt("a*"), "ab"))).
test("a*c-abc", (phrase(patt("a*c"), "abc"))).
test("*-abc",   (phrase(patt("*"), "abc"))).
test("***-abc", (phrase(patt("***"), "abc"))).

test("*?-a",    (phrase(patt("*?"), "a"))).
test("*?*-a",   (phrase(patt("*?*"), "a"))).
test("?*?-ab",  (phrase(patt("?*?"), "ab"))).
test("?*?-abc", (phrase(patt("?*?"), "abc"))).
test("?*-abcde",(phrase(patt("?*"), "abcde"))).
test("?*-a",    (phrase(patt("?*"), "a"))).

test("\\?-?",    (phrase(patt("\\?"), "?"))).
test("\\?\\?-??", (phrase(patt("\\?\\?"), "??"))).
test("\\*-*",    (phrase(patt("\\*"), "*"))).
test("\\*\\*-**", (phrase(patt("\\*\\*"), "**"))).

main :-
  findall(test(Name, Goal), test(Name, Goal), Tests),
  run_tests(Tests, Failed),
  show_failed(Failed),
  halt.

run_tests([], []).
run_tests([test(Name, Goal)|Tests], Failed) :-
  format("Running test \"~s\"~n", [Name]),
  (   call(Goal) ->
    Failed = Failed1
  ;   format("Failed test \"~s\"~n", [Name]),
    Failed = [Name|Failed1]
  ),
  run_tests(Tests, Failed1).

show_failed(Failed) :-
  phrase(portray_failed(Failed), F),
  format("~s", [F]).

portray_failed_([]) --> [].
portray_failed_([F|Fs]) -->
  "\"", F, "\"",  "\n", portray_failed_(Fs).

portray_failed([]) --> [].
portray_failed([F|Fs]) -->
  "\n", "Failed tests:", "\n", portray_failed_([F|Fs]).

:- initialization(main).
