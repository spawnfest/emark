-ifndef(EMARK_INTERNAL_HRL).
-define(EMARK_INTERNAL_HRL, awww_yeah).

%% default emark work dir
-define(EMARK_DIR, ".emark").

%% suffix which a func need to have in order to be benchmarked
-define(DEFAULT_FUN_SUFFIX, "_benchmark").

%% arity of benchmarking functions
-define(BENCH_FUN_ARITY, 1).

%% default number of iterations per function
-define(BENCH_DEFAULT_N, 10).

%% default benchmark time -- 1 second
-define(BENCH_DEFAULT_TIME, 1000000).

%% report to stdout by default
-define(BENCH_DEFAULT_REPORT_STDOUT, true).

%% default report filename
-define(BENCH_DEFAULT_REPORT_FILE, "report.txt").

%% show diff by default
-define(BENCH_DEFAULT_SHOW_DIFF, true).

-endif. % !EMARK_INTERNAL_HRL
