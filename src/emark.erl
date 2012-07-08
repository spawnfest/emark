%% @doc Public interface to emark.

-module(emark).

-export([ start/1
        , start/3
        ]).

%% @doc Start benchmarking specified function defined
%% using { Module, Function, Arity }.
-spec start({ atom(), atom(), non_neg_integer() }) ->
               ok | no_return().
start({ _M, _F, _A } = MFA) ->
  %% send the MFA so the tracer process can match on it later
  self() ! { function, MFA },
  %% call      -- trace calls
  %% return_to -- trace actual return from a call
  Ok = ((erlang:trace(existing, true, [ call, return_to ]) > 0)
        and (erlang:trace_pattern(MFA, true, [ local, call_count ]) > 0)),
  case Ok of
    true ->
      self() ! { started, os:timestamp() },
      ok;
    _ ->
      throw(trace_failed)
  end.

%% @doc Start benchmarking specified function defined
%% using Module, Function and Arity.
-spec start(atom(), atom(), non_neg_integer()) ->
               ok | no_return().
start(M, F, A) ->
  start({ M, F, A }).
