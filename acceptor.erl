-module(acceptor).
-export([start/0]).

start() ->
  next(-1, []).

next(Ballot_num, Accepted) ->
  receive
    {phase1a, Leader_id, Ballot} ->
      if
        Ballot > Ballot_num -> Ballot_num2 = Ballot;
        true                -> Ballot_num2 = Ballot_num
      end,

      Leader ! {phase1b, self(), Ballot_num2, Accepted},
      next(Ballot_num2, Accepted);


    {phase2a, Leader_id, {Ballot, Slot_num, Command}} ->
      if
        Ballot == Ballot_num ->
          Accepted2 = Accepted ++ `[{Ballot, Slot_num, Command}];
        true ->
          Accepted2 = Accepted
      end,

      Leader ! {phase2b, self(), Ballot_num},
      next(Ballot_num, Accepted2)

  end.
