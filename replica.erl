
%%% distributed algorithms, n.dulay 27 feb 17
%%% coursework 2, paxos made moderately complex

-module(replica).
-export([start/1]).

start(Database) ->
  receive
    {bind, Leaders} ->
       next(Database, 1, 1, sets:new(), maps:new(), maps:new(), Leaders)
  end.

next(State, Slot_in, Slot_out, Requests, Proposals, Decisions, Leaders) ->
  receive
    {request, C} ->      % request from client
      Requests2 = sets:add_element(C, Requests),
      {Slot_out2, Decisions2, Slot_in2, Proposals2} =
        {Slot_out, Decisions, Slot_in, Proposals};

    {decision, S, C} ->  % decision from commander
      Decisions2 = maps:put(S, C, Decisions),
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

    Slot_in_map = maps:get(Slot_in, Decisions, -1) /= -1,
    if not Slot_in_map ->
      Requests2  = sets:del_element(C, Requests),
      Proposals2 = maps:put(Slot_in, C, Proposals),
      [ Leader ! {propose, Slot_in, C} || Leader <- Leaders];
    true -> {Requests2, Proposals2} = {Requests, Proposals}
    end,

    Slot_in2 = Slot_in + 1,
    propose(Slot_in2, Slot_out, Proposals2, Requests2, Leaders, Decisions);

  true -> {Slot_in, Proposals, Requests}
  end.

decide(Decisions, Database, Proposals, Requests, Slot_out) ->
  DC = maps:get(Slot_out, Decisions, -1),

  if DC == -1 -> {Slot_out, Proposals, Requests};
  true        ->
    PC = maps:get(Slot_out, Proposals, -1),
    Requests2 =
      if (PC /= DC) and (PC /= -1) -> sets:add_element(PC, Requests);
         true                      -> Requests
      end,

    Proposals2 = maps:remove(Slot_out, Proposals),
    Slot_out2 = perform(Database, DC, Decisions, Slot_out),
    decide(Decisions, Database, Proposals2, Requests2, Slot_out2)
  end.

perform(Database, {Client, Cid, Op}, Decisions, Slot_out) ->
  Has_lower = has_lower_slot(Decisions, Slot_out),
  Slot_out2 = if Has_lower ->
                Slot_out + 1;
              true ->
                Slot_out
              end,
  Database ! {execute, Op},
  Slot_out3 = Slot_out2 + 1,
  Client ! {response, Cid, ok},
  Slot_out3.

has_lower_slot(Decisions, Slot) ->
  has_lower_slot(Decisions, Slot, Slot - 1).

has_lower_slot(_, _, 0) -> false;

has_lower_slot(Decisions, Slot, Curr) ->
  case (maps:get(Curr, Decisions, -1) /= -1) of
    true -> true;
    false-> has_lower_slot(Decisions, Slot, Curr - 1)
  end.
