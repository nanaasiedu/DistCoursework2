
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
      {Decisions2, Slot_in2, Proposals2} = {Decision, Slot_in, Proposals},
      Slot_out2 = Slot_out;

    {decision, S, C} ->  % decision from commander
      Decisions2 = Decisions ++ [{S, C}],
      {Slot_out2, Proposals2, Requests2} = decide (Decisions2),
      Slot_in2 = Slot_in

  end, % receive

  {Slot_in3, Proposals3, Requests3} = propose(Slot_in2, Slot_out2, Proposals2,
                                              Requests2, Leaders, Decisions2),
  next(State, Slot_in3, Slot_out2, Request3, Proposals3, Decision2, Leaders).

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

decide(Decisions, Database, Proposals, Requests, Slot_out) ->
  C_Slotout_D = [ C | {S, C} <- Decisions, S == Slot_out ],
  C_Slotout_P = [ {S, C} | {S, C} <- Proposals, S == Slot_out ]
  decide(Decisions, Database, Proposals, Requests, Slot_out,
         C_Slotout_D, C_Slotout_P).

decide(Decisions, Database, Proposals, Requests, Slot_out,
       [C | Rest], []) ->
  Slot_out2 = perform(Database, C, Decision, Slot_out),
  decide(Decisions, Database, Proposals, Requests, Slot_out2,
         Rest, []).

decide(Decisions, Database, Proposals, Requests, Slot_out, [], _) ->
  {Slot_out, Proposals, Requests};

decide(Decisions, Database, Proposals, Requests, Slot_out,
       [C | RestD], [ {S, Cp} | RestP]) ->
  Proposals2 = Proposals -- [{S, Cp}],
  Requests2 =
    if C /= Cp ->
      Requests ++ [Cp];
    true ->
      Requests
    end,
  Slot_out2 = perform(Database, C, Decision, Slot_out),
  decide(Decisions, Database, Proposals2, Requests2, Slot_out2,
         RestD, RestP).

perform(Database, {Client, Op, Cid}, Decisions, Slot_out) ->
  Slot_out2 = if has_lower_slot(Slot_out, {Client, Op, Cid}, Decisions) ->
                Slot_out + 1;
              true ->
                Slot_out
              end,
  Database ! {execute, Op},
  Slot_out3 = Slot_out2 + 1,
  Client ! {response, Cid, ok},
  Slot_out3.

has_lower_slot(Slot_out, C, [ {S, Cp} | Ds]) ->
  if
    S < Slot_out and C == Cp -> true;
    true                     -> has_lower_slot(Slot_out, C, Ds)
  end;

has_lower_slot(Slot_out, C, []) -> false.

slot_in_lists(Slot, []) -> false;

slot_in_lists(Slot, [{Slot, _} | _]) -> true;

slot_in_lists(Slot, [H | T]) -> slot_in_lists(Slot, T).
