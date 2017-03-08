-module(acceptor).
-export([start/0]).

start() ->
  next(-1, sets:new()).

next(Ballot_num, Accepted) ->
  receive
    {phase1a, Scout, Ballot} ->
      if
        Ballot > Ballot_num -> Ballot_num2 = Ballot;
        true                -> Ballot_num2 = Ballot_num
      end,

      Scout ! {phase1b, self(), Ballot_num2, Accepted},
      next(Ballot_num2, Accepted);

    {phase2a, Commander, {Ballot, Slot_num, Command}} ->
      if
        Ballot == Ballot_num ->
          Accepted2 = sets:add_element({Ballot, Slot_num, Command}, Accepted);
        true ->
          Accepted2 = Accepted
      end,

      Commander ! {phase2b, self(), Ballot_num},
      next(Ballot_num, Accepted2)

  end.
