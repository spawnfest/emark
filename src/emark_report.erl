-module(emark_report).

-export([ from_file/1
        , to_file/2
        , to_stdout/1
        ]).

from_file(Filename) ->
  { ok, Data } = file:read_file(Filename),

  F = fun([ Func, Arity, Count, Time ]) ->
          { list_to_atom(Func)
          , list_to_integer(Arity)
          , list_to_integer(Count)
          , list_to_float(Time)
          }
      end,

  case re:run(Data, "\([^/]+\)/([0-9]+)\t([^ \t]+)\t([^ \t]+)[^\n]+\n",
              [ global, { capture, [ 1, 2, 3, 4 ], list }, unicode ]) of
    { match, Keys } ->
      lists:map(F, Keys);

    nomatch ->
      { error, invalid_report }
  end.

to_file(Report, Filename) ->
  file:write_file(Filename, to_string(Report)).

to_stdout(Report) ->
  io:format("~s", [ to_string(Report) ]).

to_string(Report) ->
  F = fun({ Func, Arity, Count, Average }) ->
          io_lib:format("~p/~p\t~p\t~.1f µs/op~n",
                        [ Func, Arity, Count, Average ])
      end,

  lists:flatmap(fun(Results) ->
                    lists:map(F, Results)
                end,
                Report).
