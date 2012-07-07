-module(emark_plugin).

-export([ emark/2
        , clean/2
        ]).

-define(EMARK_DIR, ".emark").

emark(Config, _AppFile) ->
  %% ensure ?EMARK_DIR and ebin dirs exist
  ok = filelib:ensure_dir(emark_dir() ++ "/lol"),
  ok = filelib:ensure_dir(ebin_dir() ++ "/lol"),

  %% setup code paths
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

  rebar_erlc_compiler:doterl_compile(emark_config(Config), ?EMARK_DIR, SrcErls).

clean(_Config, _File) ->
  rebar_file_utils:rm_rf(?EMARK_DIR).

%===============================================================================

emark_dir() ->
  filename:join(rebar_utils:get_cwd(), ?EMARK_DIR).

ebin_dir() ->
  filename:join(rebar_utils:get_cwd(), "ebin").

emark_config(Config0) ->
  ErlOpts = rebar_config:get_list(Config0, erl_opts, []),
  EmarkOpts = rebar_config:get_list(Config0, emark_compile_opts, []),
  Opts0 = [ { d, 'TEST' } ] ++ ErlOpts ++ EmarkOpts,
  Opts1 = [ Opt || Opt <- Opts0, Opt =/= no_debug_info ],
  Config1 = rebar_config:set(Config0, erl_opts, Opts1),
  FirstErls = rebar_config:get_list(Config1, emark_first_files, []),
  rebar_config:set(Config1, erl_first_files, FirstErls).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
