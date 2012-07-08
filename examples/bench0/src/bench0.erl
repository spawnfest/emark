-module(bench0).

-export([ parse_omg_wtf/1
        , calc_something/0
        ]).

-include_lib("emark/include/emark.hrl").

parse_omg_wtf(X) ->
  (X * X * X - X * X - X) / (X / 3) / (3 / X).

calc_something() ->
  crypto:rand_bytes(1024).

-ifdef(BENCHMARK).

calc_something_benchmark(N) ->
  F = fun(F, 0) ->
          ok;
          (F, X) ->
          _ = calc_something(),
          F(F, X - 1)
      end,

  emark:start({ ?MODULE, calc_something, 0 }),
  F(F, N).

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
