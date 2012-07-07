-module(emark_strip_benchmark).

-export([ parse_transform/2
        ]).

parse_transform(Forms, Options) ->
  Suffix = emark_utils:fun_suffix(Options),
  Exports = emark_utils:exports_of_forms(Forms),

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
