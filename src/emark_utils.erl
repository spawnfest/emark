%% @doc Misc utils.

-module(emark_utils).

-export([ ceil/1
        , exports_of_forms/1
        , exports_of_forms/2
        , fun_suffix/1
        ]).

-include("emark_internal.hrl").

%% @doc Round a number to a greater one, looking a lot nicer.
ceil(X) ->
  Floor = floor_10(X),

  case lists:dropwhile(fun(N) ->
                           X >= N * Floor
                       end,
                       [ 2, 3, 5 ]) of
    [ Mul | _ ] ->
      Mul * Floor;

    _ ->
      10 * Floor
  end.

%% @doc Exports without those with "_benchmark" suffix.
exports_of_forms(Forms) ->
  exports_of_forms(Forms, no_autoexport).

%% @doc Exports, including those with "_benchmark" suffix.
exports_of_forms(Forms, Suffix) ->
  F = fun({ attribute, _, export, ExList }, Accu) ->
          sets:union(sets:from_list(ExList), Accu);

         ({ function, _, Name, ?BENCH_FUN_ARITY, _ }, Accu)
          when is_list(Suffix) ->
          case lists:suffix(Suffix, atom_to_list(Name)) of
            true ->
              sets:add_element({ Name, ?BENCH_FUN_ARITY }, Accu);
            false ->
              Accu
          end;

         (_, Accu) ->
          Accu
      end,

  lists:foldl(F, sets:new(), Forms).

%% @doc Round a number to a lower one.
floor_10(X) ->
  trunc(math:pow(10, trunc(math:log10(X)))).

%% @doc Get function suffix ("_benchmark") from emark options.
fun_suffix(Options) ->
  proplists:get_value(emark_fun_suffix, Options, ?DEFAULT_FUN_SUFFIX).
