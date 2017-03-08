
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
      Request2 = Request ++ [C],
      {Decisions2, Slot_in2, Proposals2} = {Decision, Slot_in, Proposals}

    {decision, S, C} ->  % decision from commander
      Decisions2 = Decisions ++ [{S, C}],
      {Slot_in2, Proposals2, Requests2} = decide (Decisions2)
  end, % receive

  ... = propose(...),
  next().

propose(Slot_in, Slot_out, Proposals, Requests, Leaders, Decisions) ->
  WINDOW = 5,
  if (Slot_in < (Slot_out + WINDOW)) and (length(Request) > 0) ->
    [C | Rest_request] = Request,

    if not slot_in_lists(Slot_in, Decisions) ->
      Requests2  = lists:delete(C, Requests),
      Proposals2 = Proposals ++ [{Slot_in, C}],
      [ Leader ! {propose, Slot_in, C} | Leader <- Leaders];
    true -> {Requests2, Proposals2} = {Requests, Proposals}
    end,

    Slot_in2 = Slot_in + 1,
    propose(Slot_in2, Slot_out, Proposals2, Requests2, Leaders, Decisions);

  true -> {Slot_in, Proposals, Requests}
  end.

decide(...) ->
  
       perform(...),
  ...

perform(...) ->
  ...
      Database ! {execute, Op},
      Client   ! {response, Cid, ok}

slot_in_lists(Slot, []) -> false;

slot_in_lists(Slot, [{Slot, _} | _]) -> true;

slot_in_lists(Slot, [H | T]) -> slot_in_lists(Slot, T).
