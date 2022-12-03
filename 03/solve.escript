#!/usr/bin/env escript

%% https://adventofcode.com/2022/day/3

-mode(compile).

main(Args) ->
  Input = read_list("~s"),
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
    fun([S], Sum) ->
        L = length(S),
        H = L div 2,
        {A, B} = lists:split(H, S),
        [C] =
          sets:to_list(
            sets:intersection(
              sets:from_list(A),
              sets:from_list(B))),
        Sum + v(C)
    end,
  lists:foldl(Fold, 0, Input).

v(C) when C >= $a, C =< $z -> C - $a + 1;
v(C) -> C - $A + 27.

solve_second(Input) ->
  Fold =
    fun([S], {C, Set, Sum}) ->
        case C of
          3 ->
            [V] =
              sets:to_list(
                sets:intersection(
                  sets:from_list(S),
                  Set)),
            {1, sets:new(), Sum + v(V)};
          2 ->
            NSet =
              sets:intersection(
                sets:from_list(S),
                Set),
            {C + 1, NSet, Sum};
          1 ->
            NSet = sets:from_list(S),
            {C + 1, NSet, Sum}
        end
    end,
  {_, _, V} = lists:foldl(Fold, {1, sets:new(), 0}, Input),
  V.
