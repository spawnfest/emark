-module(emark).

-export([ start/1
        , start/3
        ]).

start({ _M, _F, _A } = MFA) ->
  %% send the MFA so the tracer process can match on it later
  self() ! { function, MFA },
  %% call      -- trace calls
  %% return_to -- trace actual return from a call
  Ok = ((erlang:trace(existing, true, [ call, return_to ]) > 0)
        and (erlang:trace_pattern(MFA, true, [ local, call_count ]) > 0)),
  case Ok of
    true ->
      ok;
    _ ->
      throw(trace_failed)
  end.

start(M, F, A) ->
  start({ M, F, A }).
