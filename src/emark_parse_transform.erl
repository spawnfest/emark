-module(emark_parse_transform).

-export([ parse_transform/2
        ]).

-include("emark_internal.hrl").

parse_transform(Forms, _Options) ->
  Forms.
