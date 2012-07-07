-module(emark_utils).

-export([ exports_of_forms/1
        , fun_suffix/1
        ]).

-include("emark_internal.hrl").

exports_of_forms(Forms) ->
  lists:foldl(fun({ attribute, _, export, ExList }, Accu) ->
                  sets:union(sets:from_list(ExList), Accu);
                 (_, Accu) ->
                  Accu
              end,
              sets:new(),
              Forms).

fun_suffix(Options) ->
  proplists:get_value(emark_fun_suffix, Options, ?DEFAULT_FUN_SUFFIX).
