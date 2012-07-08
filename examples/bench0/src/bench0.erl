-module(bench0).

-export([ parse_omg_wtf/1
        ]).

-include_lib("emark/include/emark.hrl").

parse_omg_wtf(X) ->
  term_to_binary((X * X * X - X * X - X) / (X / 3) / (3 / X)).

-ifdef(BENCHMARK).

parse_omg_wtf_benchmark(N) ->
  Input = lists:map(fun(_) ->
                        crypto:rand_uniform(0, 16#ffffffff)
                    end,
                    lists:duplicate(N, 0)),

  emark:start({ ?MODULE, parse_omg_wtf, 1 }),

  lists:foreach(fun(X) ->
                    _ = parse_omg_wtf(X)
                end,
                Input),

  ok.

-endif.
