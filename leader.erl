-module(scout).
-export([start/0, scout/3, commandor/0]).

scout(Leader_pid, Acceptors, Ballot) ->
  [Acceptor ! {phase1a, self(), Ballot} | Acceptor <- Acceptors],
  scout(Leader_pid, Acceptors, Ballot, Acceptors, []).

scout(Leader_pid, Acceptors, Ballot, WaitFor, Pvalues) ->
  receive
    {phase1b, Acceptor, A_Ballot, Accepted} ->
      if
        Ballot == A_Ballot ->
          Pvalues2 = Pvalues ++ Accepted,
          WaitFor  = lists:delete(Acceptor, WaitFor),

          if len(WaitFor) < len(Acceptors)/2 ->
            Leader_pid ! {adopted, Ballot, Pvalues},
            exit();
          true -> nothing
          end;

        true ->
          Leader_pid ! {preempted, A_Ballot},
          exit()
      end.
  end.
