#!/usr/bin/env escript

%% https://adventofcode.com/2022/day/6

-mode(compile).

main(Args) ->
  Input = read_lines(),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~p~n", [Ans]).

read_lines() -> read_lines([]).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> lists:append(lists:reverse(Acc));
    Res -> read_lines([Res|Acc])
  end.

solve_first([A,B,C,D|Input]) ->
  solve_first(A, B, C, D, Input, 4).

solve_first(A, B, C, D, [E|R], N) ->
  case length(lists:usort([A,B,C,D])) =:= 4 of
    true -> N;
    false -> solve_first(B, C, D, E, R, N + 1)
  end.

solve_second(Input) ->
  solve_second(Input, 14).

solve_second([_|R] = Input, N) ->
  {Front, _} = lists:split(14, Input),
  case length(lists:usort(Front)) =:= 14 of
    true -> N;
    false -> solve_second(R, N + 1)
  end.
      
