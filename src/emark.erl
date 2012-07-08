-module(emark).

-export([ start/1
        , start/3
        ]).

start({ _M, _F, _A } = MFA) ->
  %% call      -- trace calls
  %% return_to -- trace actual return from a call
  %% arity     -- trace as { M, F, Arity } instead of { M, F, Args }
  X = erlang:trace(self(), true, [ call, return_to, arity ]),
  Y = erlang:trace_pattern(MFA, true, [ local, call_count, call_time ]),
  case X + Y of
    N when N > 1 ->
      ok;
    _ ->
      throw(trace_failed)
  end.

start(M, F, A) ->
  start({ M, F, A }).
