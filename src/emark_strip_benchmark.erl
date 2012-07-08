-module(emark_strip_benchmark).

-export([ parse_transform/2
        ]).

-include("emark_internal.hrl").

parse_transform(Forms, Options) ->
  Suffix = emark_utils:fun_suffix(Options),
  Exports = emark_utils:exports_of_forms(Forms),

  F = fun({ function, _, Name, ?BENCH_FUN_ARITY, _ } = Form, Accu) ->
          NameList = atom_to_list(Name),
          case (not sets:is_element({ Name, ?BENCH_FUN_ARITY }, Exports)
                andalso lists:suffix(Suffix, NameList)) of
            true ->
              Accu;
            false ->
              [ Form | Accu ]
          end
      end,

  lists:reverse(lists:foldl(F, [], Forms)).
