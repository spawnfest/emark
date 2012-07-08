-module(emark_report).

-export([ to_file/2
        , to_stdout/1
        ]).

to_file(_Report, _Filename) ->
  ok.

to_stdout(Report) ->
  F = fun({ Func, Arity, Count, Average }) ->
          io:format("~p/~p\t~p\t~.1f µs/op~n",
          [ Func, Arity, Count, Average ])
      end,

  lists:foreach(fun({ _M, Results }) ->
                    lists:foreach(F, Results)
                end,
                Report),
  ok.
