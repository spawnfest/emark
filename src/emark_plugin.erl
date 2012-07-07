-module(emark_plugin).

-export([ emark/2
        ]).

emark(_Config, _AppFile) ->
  rebar_log:log(warn, "OMG LOL WTF~n", []).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
