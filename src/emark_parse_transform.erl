-module(emark_parse_transform).

-export([ parse_transform/2
        ]).

parse_transform(Forms, Options) ->
  Suffix = emark_utils:fun_suffix(Options),
  Exports = emark_utils:exports_of_forms(Forms, Suffix),

  transform(Forms, Exports, Suffix).

transform([ { attribute, _, module, _ } = MDecl | Forms ], Exports, Suffix) ->
  module_decl(MDecl, Forms, Exports, Suffix);
transform([ Form | Forms ], Exports, Suffix) ->
  [ Form | transform(Forms, Exports, Suffix) ];
transform([], _Exports, _Suffix) ->
  [].

module_decl({ attribute, _, module, { Module, _ } } = M,
            Forms,
            Exports,
            Suffix) ->
  module_autoexport(Module, M, Forms, Exports, Suffix);
module_decl({ attribute, _, module, Module } = M,
            Forms,
            Exports,
            Suffix) ->
  module_autoexport(Module, M, Forms, Exports, Suffix).

funcs_cons([ { Func, 0 } | Funcs ]) ->
  { cons, 0
  , { 'fun', 0
    , { function, Func, 0 }
    }
  , funcs_cons(Funcs)
  };
funcs_cons([]) ->
  { nil, 0 }.

module_autoexport(_Module, M, Forms, Exports, Suffix) ->
  ExportsList = sets:to_list(Exports),

  Funcs = lists:filter(fun({ Name, _ }) ->
                           lists:suffix(Suffix, atom_to_list(Name))
                       end,
                       ExportsList),

  rebar_log:log(debug, "Benchmark funcs: ~p~n", [ Funcs ]),

  Benchmark = { function, 0, benchmark, 0
              , [ { clause, 0, [], []
                  , [ funcs_cons(Funcs) ]
                  }
                ]
              },

  Ex = { attribute, 0, export, [ { benchmark, 0 } | ExportsList ] },

  [ M, Ex, Benchmark | remove_exports(Forms) ].

remove_exports([ { attribute, _, export, _ } | Forms ]) ->
  remove_exports(Forms);
remove_exports([ Form | Forms ]) ->
  [ Form | Forms ];
remove_exports([]) ->
  [].

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
