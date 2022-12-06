#!/usr/bin/env escript

%% https://adventofcode.com/2022/day/4

-mode(compile).

main(Args) ->
  Input = read_list("~d-~d,~d-~d"),
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
    fun([AL,AH,BL,BH], Score) ->
        if
          AL =< BL, AH >= BH -> Score + 1;
          BL =< AL, BH >= AH -> Score + 1;
          true -> Score
        end
    end,
  lists:foldl(Fold, 0, Input).

solve_second(Input) ->
  Fold =
    fun([AL,AH,BL,BH], Score) ->
        Add =
          if
            AH < BL -> 0;
            BH < AL -> 0;
            true -> 1
          end,
        Score + Add
    end,
  lists:foldl(Fold, 0, Input).
