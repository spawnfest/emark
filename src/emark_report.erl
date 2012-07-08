-module(emark_report).

-export([ to_file/2
        , to_stdout/1
        ]).

to_file(Report, Filename) ->
  file:write_file(Filename, to_string(Report)).

to_stdout(Report) ->
  io:format("~s", [ to_string(Report) ]).

to_string(Report) ->
  F = fun({ Func, Arity, Count, Average }) ->
          io_lib:format("~p/~p\t~p\t~.1f µs/op~n",
                        [ Func, Arity, Count, Average ])
      end,

  lists:flatmap(fun({ _M, Results }) ->
                    lists:map(F, Results)
                end,
                Report).
