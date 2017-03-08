
%%% distributed algorithms, n.dulay 27 feb 17
%%% coursework 2, paxos made moderately complex

-module(replica).
-export([start/1]).

start(Database) ->
  receive
    {bind, Leaders} ->
       next(Database, 1, 1, sets:new(), [], [], Leaders)
  end.

next(State, Slot_in, Slot_out, Requests, Proposals, Decisions, Leaders) ->
  receive
    {request, C} ->      % request from client
      Requests2 = sets:add_element(C, Requests),
      {Decisions2, Slot_in2, Proposals2} = {Decisions, Slot_in, Proposals},
      Slot_out2 = Slot_out;

    {decision, S, C} ->  % decision from commander
      Decisions2 = Decisions ++ [{S, C}],
      {Slot_out2, Proposals2, Requests2}
        = decide (Decisions2, State, Proposals, Requests, Slot_out),
      Slot_in2 = Slot_in

  end, % receive

  {Slot_in3, Proposals3, Requests3} = propose(Slot_in2, Slot_out2, Proposals2,
                                              Requests2, Leaders, Decisions2),
  next(State, Slot_in3, Slot_out2, Requests3, Proposals3, Decisions2, Leaders).

propose(Slot_in, Slot_out, Proposals, Requests, Leaders, Decisions) ->
  WINDOW = 5,
  RequestSize = sets:size(Requests),
  if (Slot_in < (Slot_out + WINDOW)) and (RequestSize > 0) ->
    C = hd(sets:to_list(Requests)),

    Slot_in_list = slot_in_lists(Slot_in, Decisions),
    if not Slot_in_list ->
      Requests2  = sets:del_element(C, Requests),
      Proposals2 = Proposals ++ [{Slot_in, C}],
      [ Leader ! {propose, Slot_in, C} || Leader <- Leaders];
    true -> {Requests2, Proposals2} = {Requests, Proposals}
    end,

    Slot_in2 = Slot_in + 1,
    propose(Slot_in2, Slot_out, Proposals2, Requests2, Leaders, Decisions);

  true -> {Slot_in, Proposals, Requests}
  end.

decide(Decisions, Database, Proposals, Requests, Slot_out) ->
  C_Slotout_D = [ C || {S, C} <- Decisions, S == Slot_out ],
  C_Slotout_P = [ {S, C} || {S, C} <- Proposals, S == Slot_out ],
  decide(Decisions, Database, Proposals, Requests, Slot_out,
         C_Slotout_D, C_Slotout_P).

decide(Decisions, Database, Proposals, Requests, Slot_out,
       [C | Rest], []) ->
  Slot_out2 = perform(Database, C, Decisions, Slot_out),
  decide(Decisions, Database, Proposals, Requests, Slot_out2,
         Rest, []);

decide(_, _, Proposals, Requests, Slot_out, [], _) ->
  {Slot_out, Proposals, Requests};

decide(Decisions, Database, Proposals, Requests, Slot_out,
       [C | RestD], [ {S, Cp} | RestP]) ->
  Proposals2 = Proposals -- [{S, Cp}],
  Requests2 =
    if C /= Cp ->
      sets:add_element(Cp, Requests);
    true ->
      Requests
    end,
  Slot_out2 = perform(Database, C, Decisions, Slot_out),
  decide(Decisions, Database, Proposals2, Requests2, Slot_out2,
         RestD, RestP).

perform(Database, {Client, Cid, Op}, Decisions, Slot_out) ->
  Has_lower = has_lower_slot(Slot_out, {Client, Op, Cid}, Decisions),
  Slot_out2 = if Has_lower ->
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
    (S < Slot_out) and (C == Cp) -> true;
    true                         -> has_lower_slot(Slot_out, C, Ds)
  end;

has_lower_slot(_, _, []) -> false.

slot_in_lists(_, []) -> false;

slot_in_lists(Slot, [{Slot, _} | _]) -> true;

slot_in_lists(Slot, [_ | T]) -> slot_in_lists(Slot, T).
