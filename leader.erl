-module(leader).
-export([start/0, scout/3, commander/4]).

start() ->
  receive
    {bind, Acceptors, Replicas} -> received
  end,

  Ballot_num = {0, self()},
  Active     = false,
  Proposals  = sets:new(),
  spawn(leader, scout, [self(), Acceptors, Ballot_num]),
  next(Acceptors, Replicas, Ballot_num, Active, Proposals).

next(Acceptors, Replicas, Ballot_num, Active, Proposals) ->
  receive
    {propose, Slot, Command} ->
      SlotNotInProposals = not slot_in_proposals(Slot, sets:to_list(Proposals)),
      if SlotNotInProposals ->
        Proposals2 = sets:add_element({Slot, Command}, Proposals),

        if Active ->
          spawn(leader, commander,
                [self(), Acceptors, Replicas, {Ballot_num, Slot, Command}]);
        true -> nothing
        end;

      true -> Proposals2 = Proposals
      end,
      next(Acceptors, Replicas, Ballot_num, Active, Proposals2);

    {adopted, A_Ballot, Pvalues} ->
      PropLists  = sets:to_list(Proposals),
      Proposals2 = sets:union(
                     [sets:from_list([{Slot, Command} ||
                        {Slot, Command} <- PropLists,
                        not slot_in_proposals(Slot, PropLists)])
                   , Pvalues]),

      [spawn(leader, commander, [self(), Acceptors, Replicas, {A_Ballot, Slot, Command}])
       || {Slot, Command} <- sets:to_list(Proposals2)],

      Active2 = true,
      next(Acceptors, Replicas, A_Ballot, Active2, Proposals2);

    {preempted, {A_Ballot, ID}} ->
      if {A_Ballot, ID} > Ballot_num ->
        Active2 = false,
        Ballot_num2 = {A_Ballot + 1, self()},
        spawn(leader, scout, [self(), Acceptors, Ballot_num2]);

      true -> Active2 = Active, Ballot_num2 = Ballot_num
      end,

      next(Acceptors, Replicas, Ballot_num2, Active2, Proposals)
  end.

slot_in_proposals(_, []) -> false;

slot_in_proposals(Slot, [{Slot, _} | _]) -> true;

slot_in_proposals(Slot, [_ | T]) -> slot_in_proposals(Slot, T).

scout(Leader_pid, Acceptors, Ballot) ->
  [Acceptor ! {phase1a, self(), Ballot} || Acceptor <- Acceptors],
  scout(Leader_pid, Acceptors, Ballot, Acceptors, sets:new()).

scout(Leader_pid, Acceptors, Ballot, WaitFor, Pvalues) ->
  receive
    {phase1b, Acceptor, A_Ballot, Accepted} ->
      if
        Ballot == A_Ballot ->
          Pvalues2  = sets:union([Pvalues, Accepted]),
          WaitFor2  = lists:delete(Acceptor, WaitFor),

          if
            length(WaitFor2) < length(Acceptors)/2 ->
              Leader_pid ! {adopted, Ballot, Pvalues2},
              exit(normal);
            true -> scout(Leader_pid, Acceptors, Ballot, WaitFor2, Pvalues2)
          end;

        true ->
          Leader_pid ! {preempted, A_Ballot},
          exit(normal)
      end
  end.

commander(Leader_pid, Acceptors, Replicas, {Ballot, Slot, Command}) ->
  [Acceptor ! {phase2a, self(), {Ballot, Slot, Command}} || Acceptor <- Acceptors],
  commander(Leader_pid, Acceptors, Replicas, {Ballot, Slot, Command}, Acceptors).

commander(Leader_pid, Acceptors, Replicas, {Ballot, Slot, Command}, WaitFor) ->
  receive
    {phase2b, Acceptor, A_Ballot} ->
      if
        A_Ballot == Ballot ->
          WaitFor2 = lists:delete(Acceptor, WaitFor),

          if
            length(WaitFor2) < length(Acceptors)/2 ->
              [Replica ! {decision, Slot, Command} || Replica <- Replicas],
              exit(normal);

            true ->
              commander(Leader_pid, Acceptors, Replicas, {Ballot, Slot, Command}, WaitFor2)
          end;

        true ->
          Leader_pid ! {preempted, A_Ballot},
          exit(normal)

      end
  end.
