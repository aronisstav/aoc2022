#!/usr/bin/env escript

%% https://adventofcode.com/2022/day/1

-mode(compile).

main(Args) ->
  Input = read_lines(),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~w~n", [Ans]).

read_lines() -> read_lines([]).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> lists:append(lists:reverse(Acc));
    Res -> read_lines([[token(Res)]|Acc])
  end.

token(List) ->
  case [C || C <- List, C =/= $\n] of
    [] -> break;
    Other -> list_to_integer(Other)
  end.

solve_first(Input) ->
  Fold =
    fun(C, {Local, Total}) ->
        case C =:= break of
          true -> {0, max(Local, Total)};
          false -> {Local + C, Total}
        end
    end,
  {Final, Max} = lists:foldl(Fold, {0, 0}, Input),
  max(Final, Max).

solve_second(Input) ->
  Fold =
    fun(C, {Local, Totals}) ->
        case C =:= break of
          true -> {0, [Local|Totals]};
          false -> {Local + C, Totals}
        end
    end,
  {Final, Totals} = lists:foldl(Fold, {0, []}, Input),
  [A, B, C|_] = lists:reverse(lists:sort([Final|Totals])),
  A + B + C.
