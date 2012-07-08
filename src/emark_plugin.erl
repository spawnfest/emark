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

benchmark(_Modules, _EmarkOpts) ->
  ok.

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

  ok = file:set_cwd(Cwd),
  EmarkResult.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
