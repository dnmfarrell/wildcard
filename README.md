# wildcard

Prolog DCG to match string patterns. Supports:

- `*` wildcard: matches any pattern including the empty string.
- `?` placeholder: matches any single atom.
- `\` escape: turns `*`, `?` and `\` into literal tokens.

N.B. this module intentionally does not verify its arguments are strings - additional clauses can be added to restrict the generated characters (examples below).

Requires [Scryer-Prolog](https://www.scryer.pl/) or similar interpreter.

## Usage

Exports `patt//1` which matches a string pattern to a string:

    $ scryer-prolog src/wildcard.pl
    ?- phrase(patt("1?3"), "123").
       true.
    ?- phrase(patt("*"), "123").
       true.
    ?- phrase(patt("***"), "123").
       true.
    ?- phrase(patt("???"), "123").
       true.
    ?- phrase(patt("?*?"), "123").
       true.

Wildcard can also generate strings for given pattern, with a helper predicate to restrict the generated characters:

    ?- [user].
    uc_char(Char) :- member(Char, "ABCDEFGHIJKLMNOPQRSTUVWXYZ").
    
    ?- phrase(patt("D?G"), Cs), maplist(uc_char, Cs).
       Cs = "DAG"
    ;  Cs = "DBG"
    ;  Cs = "DCG"
    ;  Cs = "DDG"
    ;  ... .

Limit the search space for wildcard patterns by length:

    ?- length(Cs, 4), phrase(patt("D*G"), Cs), maplist(uc_char, Cs).
       Cs = "DAAG"
    ;  Cs = "DABG"
    ;  Cs = "DACG"
    ;  Cs = "DADG"
    ;  Cs = "DAEG"
    ;  ... .

## Testing

    $ scryer-prolog -f test/wildcard.pl
    Running test "a-ab"
    Running test "A-a"
    Running test "aB-ab"
    Running test "abC-abc"
    Running test "a-a"
    Running test "ab-ab"
    Running test "?a-a"
    Running test "a?-a"
    Running test "?-a"
    Running test "a?-ab"
    Running test "a?c-abc"
    Running test "???-abc"
    Running test "*a-a"
    Running test "a*-a"
    Running test "*-a"
    Running test "a*-ab"
    Running test "a*c-abc"
    Running test "*-abc"
    Running test "***-abc"
    Running test "*?-a"
    Running test "*?*-a"
    Running test "?*?-ab"
    Running test "?*?-abc"
    Running test "?*-abcde"
    Running test "?*-a"
    Running test "\?-?"
    Running test "\?\?-??"
    Running test "\*-*"
    Running test "\*\*-**"
    Running test "\A-A"
    Running test "\A-\A"
