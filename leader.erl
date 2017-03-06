-module(leader).
-export([start/0, scout/3, commander/4]).

start(Acceptors, Replicas) ->
  Ballot_num = {0, self()},
  Active     = false,
  Proposals  = [],
  spawn(leader, scout, [self(), Acceptors, Ballot_num]),
  next(Acceptors, Replicas, Ballot_num, Active, Proposals).

next(Acceptors, Replicas, Ballot_num, Active, Proposals) ->
  receive
    {propose, Slot, Command} ->
      NumSlotProposals = len([1 || P <- Proposals, P = {Slot, _}]),
      if NumSlotProposals == 0 ->
        Proposals2 = Proposals ++ [{Slot, Command}],

        if Active ->
          spawn(leader, commander,
                [self(), Acceptors, Replicas, {Ballot_num, Slot, Command}]);
        true -> nothing
        end;

      true -> Proposals2 = Proposals
      end,
      next(Acceptors, Replicas, Ballot_num, Active, Proposals2);

    {adopted, A_Ballot, Pvalues} ->
      Proposals2 = [{Slot, Command} || {Slot, Command} <- Proposals,
                                      len([1 || P <- Proposals, P = {Slot, _}]) == 0]
                   ++ Pvalues,

      [spawn(leader, commander, [self(), Acceptors, Replicas, {Ballot_num, Slot, Command}])
       || {Slot, Command} <- Proposals2],

      Active2 = true,
      next(Acceptors, Replicas, Ballot_num, Active2, Proposals2);

    {preempted, {Replica, {A_Ballot, ID}}} ->
      {Ballot, _}   = Ballot_num, %CHECKKK

      if A_Ballot > Ballot ->
        Active2 = false,
        Ballot_num2 = {A_Ballot + 1, self()},
        spawn(leader, scout, [self(), Acceptors, Ballot_num2]);

      true -> Active2 = Active, Ballot_num2 = Ballot_num
      end,

      next(Acceptors, Replicas, Ballot_num2, Active2, Proposals)
  end.

scout(Leader_pid, Acceptors, Ballot) ->
  [Acceptor ! {phase1a, self(), Ballot} | Acceptor <- Acceptors],
  scout(Leader_pid, Acceptors, Ballot, Acceptors, []).

scout(Leader_pid, Acceptors, Ballot, WaitFor, Pvalues) ->
  receive
    {phase1b, Acceptor, A_Ballot, Accepted} ->
      if
        Ballot == A_Ballot ->
          Pvalues2  = Pvalues ++ Accepted,
          WaitFor2  = lists:delete(Acceptor, WaitFor),

          if
            len(WaitFor2) < len(Acceptors)/2 ->
              Leader_pid ! {adopted, Ballot, Pvalues2},
              exit();
            true -> scout(Leader_pid, Acceptors, Ballot, WaitFor2, Pvalues2)
          end;

        true ->
          Leader_pid ! {preempted, A_Ballot},
          exit()
      end.
  end.

commander(Leader_pid, Acceptors, Replicas, {Ballot, Slot, Command}) ->
  [Acceptor ! {phase2a, self(), {Ballot, Slot, Command}} | Acceptor <- Acceptors],
  commander(Leader_pid, Acceptors, Replicas, {Ballot, Slot, Command}, Acceptors).

commander(Leader_pid, Acceptors, Replicas, {Ballot, Slot, Command}, WaitFor) ->
  receive
    {phase2b, Acceptor, A_Ballot} ->
      if
        A_Ballot == Ballot ->
          WaitFor2 = lists:delete(Acceptor, Waitfor),

          if
            len(WaitFor2) < len(Acceptors)/2 ->
              [Replica ! {decision, Slot, Command} | Replica <- Replicas],
              exit();

            true ->
              commander(Leader_pid, Acceptors, Replicas, {Ballot, Slot, Command}, WaitFor2)
          end;

        true ->
          Leader_pid ! {preempted, A_Ballot},
          exit()

      end
  end.
