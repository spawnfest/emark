-module(bench0).

-export([ parse_omg_wtf/1
        ]).

-include_lib("emark/include/emark.hrl").

parse_omg_wtf(X) ->
  term_to_binary((X * X * X - X * X - X) / (X / 3) / (3 / X)).

-ifdef(BENCHMARK).

parse_omg_wtf_benchmark() ->
  ok.

-endif.
