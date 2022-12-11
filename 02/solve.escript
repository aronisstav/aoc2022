#!/usr/bin/env escript

%% https://adventofcode.com/2022/day/2

-mode(compile).

main(Args) ->
  Input = read_list("~c ~c"),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~w~n", [Ans]).

read_list(Pat) -> read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

solve_first(Input) ->
  Fold =
    fun([[Opp], Me], Score) ->
        Score + move_points(Me) + match_points(Me, [Opp - $A + $X])
    end,
  lists:foldl(Fold, 0, Input).

move_points("X") -> 1;
move_points("Y") -> 2;
move_points("Z") -> 3.

match_points(A, A) -> 3;
match_points("X", "Z") -> 6;
match_points("Y", "X") -> 6;
match_points("Z", "Y") -> 6;
match_points(_, _) -> 0.

solve_second(Input) ->
  Fold =
    fun([[Opp], Res], Score) ->
        Me = choose(Res, [Opp]),
        Score + move_points(Me) + match_points(Me, [Opp - $A + $X])
    end,
  lists:foldl(Fold, 0, Input).

choose("Y", [M]) -> [M - $A + $X];
choose("Z", "A") -> "Y";
choose("Z", "B") -> "Z";
choose("Z", "C") -> "X";
choose("X", "A") -> "Z";
choose("X", "B") -> "X";
choose("X", "C") -> "Y".
