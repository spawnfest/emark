-module(emark_report).

-export([ from_file/1
        , to_file/2
        , to_stdout/1
        , show_diff/2
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

  lists:flatmap(F, Report).

show_diff(Old0, New0) ->
  Old = lists:sort(Old0),
  New = lists:sort(New0),

  Delta = fun(X0, Y0) ->
              X = trunc(X0 * 10.0) / 10.0,
              Y = trunc(Y0 * 10.0) / 10.0,
              Diff = 100 * (1.0 - (X / Y)),
              io_lib:format(case X > Y of
                              true  -> "~.2f%";
                              false -> "+~.2f%"
                            end,
                            [ Diff ])
          end,

  Cmp = fun({ F, A, _OldC, OldT }, { F, A, _NewC, NewT }) ->
            Func = lists:flatten(io_lib:format("~p/~B", [ F, A ])),
            io:format("~-30s\t~10.1f\t~10.1f\t~8s~n",
                      [ Func, OldT, NewT, Delta(OldT, NewT) ])
        end,

  io:format("~-30s\t~11s\t~11s\t~8s~n",
            [ "benchmark", "old µs/op", "new µs/op", "delta" ]),
  lists:zipwith(Cmp, Old, New),
  ok.
