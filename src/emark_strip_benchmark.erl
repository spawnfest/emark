-module(emark_strip_benchmark).

-export([ parse_transform/2
        ]).

-include("emark_internal.hrl").

parse_transform(Forms, Options) ->
  Suffix = proplists:get_value(emark_fun_suffix, Options, ?DEFAULT_FUN_SUFFIX),

  Exports = lists:foldl(fun({ attribute, _, export, ExList }, Accu) ->
                            sets:union(sets:from_list(ExList), Accu);
                           (_, Accu) ->
                            Accu
                        end,
                        sets:new(),
                        Forms),

  F = fun({ function, _, Name, 0, _ } = Form, Accu) ->
          NameList = atom_to_list(Name),
          case (not sets:is_element({ Name, 0 }, Exports)
                andalso lists:suffix(Suffix, NameList)) of
            true ->
              Accu;
            false ->
              [ Form | Accu ]
          end
      end,

  lists:reverse(lists:foldl(F, [], Forms)).
