#!/usr/bin/env escript

%% https://adventofcode.com/2022/day/10

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
    eof -> lists:reverse(Acc);
    Res ->
      [_|R] = lists:reverse(Res),
      read_lines([lists:reverse(R)|Acc])
  end.

solve_first(Input) ->
  sig(Input, #{x => 1, c => 0, r => 0, sum => 0, go => false}).

sig(Input, State) ->
  %io:format("~p~n", [{Input, State}]),
  sig1(Input, State).

sig1(Input, #{r := N} = State) when N > 0 ->
  sig(Input, clock(State#{r => N - 1, go => true}));
sig1([], State) ->
  State;
sig1(["addx " ++ Num|Rest], #{go := true} = State) ->
  N = list_to_integer(Num),
  #{ x := X } = State,
  sig(Rest, State#{x => X + N, go => false});
sig1([Op|Rest] = Ops, State) ->
  case Op of
    "noop" ->
      sig(Rest, clock(State));
    "addx " ++ _ ->
      sig(Ops, clock(State#{r => 1}))
  end.

clock(#{draw := true, c := C, p := P, x := X} = State) ->
  NC = C + 1,
  Ch =
    case abs(P - X) =< 1 of
      true -> $#;
      false -> $
    end,
  io:format("~c", [Ch]),
  NP =
    case P =:= 39 of
      true ->
        io:format("~n"),
        0;
      false ->
        P + 1
    end,
  State#{C => NC, p => NP};
clock(#{c := C} = State) ->
  NC = C + 1,
  case NC rem 40 =:= 20 of
    true ->
      #{x := X, sum := Sum} = State,
      State#{sum => Sum + NC * X, c => NC};
    false ->
      State#{c => NC}
  end.

solve_second(Input) ->
  sig(Input, #{x => 1, c => 0, r => 0, sum => 0, go => false, draw => true, p => 0}).
