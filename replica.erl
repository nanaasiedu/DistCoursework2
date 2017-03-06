
%%% distributed algorithms, n.dulay 27 feb 17
%%% coursework 2, paxos made moderately complex

-module(replica).
-export([start/1]).

start(Database) ->
  receive
    {bind, Leaders} ->
       next(Database, 1, 1, [], [], [], Leaders)
  end.

next(State, Slot_in, Slot_out, Request, Proposals, Decision, Leaders) ->
  receive
    {request, C} ->      % request from client
      ...
    {decision, S, C} ->  % decision from commander
      ... = decide (...)
  end, % receive

  ... = propose(...),
  ...

propose(...) ->
  WINDOW = 5,
  ...

decide(...) ->
  ...
       perform(...),
  ...

perform(...) ->
  ...
      Database ! {execute, Op},
      Client   ! {response, Cid, ok}
