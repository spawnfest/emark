-module(emark_plugin).

-export([ emark/2
        , clean/2
        ]).

-include("emark_internal.hrl").

emark(Config, _AppFile) ->
  %% ensure ?EMARK_DIR and ebin dirs exist
  ok = filelib:ensure_dir(emark_dir() ++ "/lol"),
  ok = filelib:ensure_dir(ebin_dir() ++ "/lol"),

  %% setup code paths
  CodePath = code:get_path(),
  true = code:add_patha(emark_dir()),
  true = code:add_pathz(ebin_dir()),

  %% copy source files to ?EMARK_DIR
  SrcDirs = rebar_config:get_list(Config, src_dirs, [ "src" ]),
  SrcErls = lists:foldl(fun(Dir, Accu) ->
                            Files = rebar_utils:find_files(Dir, ".*\\.erl\$"),
                            Accu ++ Files
                        end,
                        [],
                        SrcDirs),

  ToCleanUp = fun(F, Accu) ->
                  F2 = filename:basename(F),
                  F3 = filename:join([ ?EMARK_DIR, F2 ]),
                  case filelib:is_regular(F3) of
                    true  -> [ F3 | Accu ];
                    false -> Accu
                  end
              end,

  ok = rebar_file_utils:delete_each(lists:foldl(ToCleanUp, [], SrcErls)),
  ok = rebar_file_utils:cp_r(SrcErls, ?EMARK_DIR),

  %% compile with -DBENCHMARK
  rebar_erlc_compiler:doterl_compile(emark_config(Config), ?EMARK_DIR, SrcErls),

  Beams = rebar_utils:beams(?EMARK_DIR),
  Modules = [ rebar_utils:beam_to_mod(?EMARK_DIR, B) || B <- Beams ],

  perform_benchmark(Config, Modules),

  %% restore code path
  true = code:set_path(CodePath),
  ok.

clean(_Config, _File) ->
  rebar_file_utils:rm_rf(?EMARK_DIR).

%===============================================================================

get_call_count(MFA) ->
  { call_count, Count } = erlang:trace_info(MFA, call_count),
  Count.

trace(B, N) ->
  Self = self(),
  spawn(fun() ->
            %% get the time it takes to run
            { Time, _ } = timer:tc(B, [ N ]),
            %% stop tracing
            erlang:trace(all, false, [ all ]),
            %% send the MFA we were benchmarking
            receive
              MFA = { function, { _, _, _ } } ->
                Self ! MFA
            end,
            %% send the time to trace_loop
            Self ! { finished, Time }
        end),
  trace_loop(B, N, undefined).

trace_loop(B, N, MFA) ->
  receive
    { function, NewMFA } ->
      trace_loop(B, N, NewMFA);

    { finished, Time } ->
      rebar_log:log(debug, "finished benchmark~n", []),
      Count = get_call_count(MFA),

      case Time of
        X when X < ?BENCH_DEFAULT_TIME ->
          %% if it's less, we should try a better number of iterations
          rebar_log:log(debug, "not enough time (~p ms vs ~p ms)~n",
                        [ trunc(X / 1000), trunc(?BENCH_DEFAULT_TIME / 1000) ]),
          Average = Time / Count,
          %% woooo.... holy crap X___x
          %% yeah, put some cool stuff here later
          NeedCount = emark_utils:ceil((?BENCH_DEFAULT_TIME * 1.1) / Average),
          %% restart the benchmark
          rebar_log:log(debug,
                        "restaring the benchmark with ~p iterations~n",
                        [ NeedCount ]),
          trace(B, NeedCount);

        _ ->
          { MFA, Count, Time }
      end;

    _ ->
      trace_loop(B, N, MFA)
  end.

run_func(_M, B, N) ->
  { { _Mod, Func, Arity }, Count, Time } = trace(B, N),
  { Func, Arity, Count, Time/Count }.

benchmark(Modules, EmarkOpts) ->
  N = proplists:get_value(n, EmarkOpts, ?BENCH_DEFAULT_N),

  RunModule = fun(M) ->
                  rebar_log:log(debug,
                                "Benchmarking ~p, ~p iterations~n",
                                [ M, N ]),
                  lists:map(fun(B) ->
                                run_func(M, B, N)
                            end,
                            M:benchmark())
              end,

  F = fun(M) ->
          { module, M } = code:load_file(M),
          case erlang:function_exported(M, benchmark, 0) of
            true ->
              RunModule(M);
            false ->
              ignore
          end
      end,

  lists:filter(fun(R) -> R /= ignore end,
               lists:map(F, Modules)).

ebin_dir() ->
  filename:join(rebar_utils:get_cwd(), "ebin").

emark_config(Config0) ->
  ErlOpts = rebar_config:get_list(Config0, erl_opts, []),
  EmarkOpts = rebar_config:get_list(Config0, emark_compile_opts, []),
  Opts0 = [ { d, 'BENCHMARK' } ] ++ ErlOpts ++ EmarkOpts,
  Opts1 = [ Opt || Opt <- Opts0, Opt =/= no_debug_info ],
  Config1 = rebar_config:set(Config0, erl_opts, Opts1),
  FirstErls = rebar_config:get_list(Config1, emark_first_files, []),
  rebar_config:set(Config1, erl_first_files, FirstErls).

emark_dir() ->
  filename:join(rebar_utils:get_cwd(), ?EMARK_DIR).

get_emark_opts(Config) ->
  rebar_config:get_list(Config, emark_opts, []).

perform_benchmark(Config, Modules) ->
  Cwd = rebar_utils:get_cwd(),
  ok = file:set_cwd(?EMARK_DIR),

  EmarkOpts = get_emark_opts(Config),
  EmarkResult = benchmark(Modules, EmarkOpts),

  %% dump to stdout
  case proplists:get_value(report_stdout,
                           EmarkOpts,
                           ?BENCH_DEFAULT_REPORT_STDOUT) of
    true  -> emark_report:to_stdout(EmarkResult);
    false -> ok
  end,

  %% dump to file
  case proplists:get_value(report_file,
                           EmarkOpts,
                           ?BENCH_DEFAULT_REPORT_FILE) of
    Filename when is_list(Filename) ->
      emark_report:to_file(EmarkResult, Filename);

    false ->
      ok
  end,

  ok = file:set_cwd(Cwd),
  EmarkResult.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
