-ifndef(EMARK_HRL).
-define(EMARK_HRL, awww_yeah).

-ifdef(BENCHMARK).
-undef(NOBENCHMARK).
-endif.

-ifndef(EMARK_NOAUTO).
-ifndef(NOBENCHMARK).
-compile({ parse_transform, emark_parse_transform }).
-else.
-compile({ parse_transform, emark_strip_benchmark }).
-endif. % !NOBENCHMARK
-endif. % !EMARK_NOAUTO

-endif. % !EMARK_HRL
