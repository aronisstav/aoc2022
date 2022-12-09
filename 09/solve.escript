#!/usr/bin/env escript

%% https://adventofcode.com/2022/day/9

-mode(compile).

main(Args) ->
  Input = read_list("~a ~d"),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~p~n", [Ans]).

read_list(Pat) -> read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

solve_first(Input) ->
  Fold = fun(Move, State) -> move(Move, State) end,
  {_,_,F} = lists:foldl(Fold, init(), Input),
  maps:size(F).

init() ->
  {{0,0}, {0,0}, #{{0,0} => true}}.

move([Dir, N], State) ->
  Fold = fun(M, St) -> move1(M, St) end,
  lists:foldl(Fold, State, lists:duplicate(N, Dir)).

move1(Dir, {Head, Tail, Visited}) ->
  NH = mh(Dir, Head),
  NT = mt(NH, Tail),
  {NH, NT, Visited#{NT => true}}.

mt({X, Y}, {A, B}) ->
  XX = X - A,
  YY = Y - B,
  case {XX, YY} of
    {-2,-2} -> {X + 1, Y + 1};
    { 2, 2} -> {X - 1, Y - 1};
    { 2,-2} -> {X - 1, Y + 1};
    {-2, 2} -> {X + 1, Y - 1};
    {-2, _} -> {X + 1, Y};
    { 2, _} -> {X - 1, Y};
    { _,-2} -> {X, Y + 1};
    { _, 2} -> {X, Y - 1};
    _ -> {A, B}
  end.      

mh('U', {X, Y}) -> {X, Y + 1};
mh('D', {X, Y}) -> {X, Y - 1};
mh('R', {X, Y}) -> {X + 1, Y};
mh('L', {X, Y}) -> {X - 1, Y}.

solve_second(Input) ->
  Fold = fun(Move, State) -> movel(Move, State) end,
  {_,F} = lists:foldl(Fold, init10(), Input),
  maps:size(F).

init10() ->
  {maps:from_list([{N, {0,0}} || N <- lists:seq(1, 10)]), #{{0,0} => true}}.

movel([Dir, N], State) ->
  Fold = fun(M, St) -> move10(M, St) end,
  lists:foldl(Fold, State, lists:duplicate(N, Dir)).

move10(Dir, {Points, Visited}) ->
  #{1 := Head} = Points,
  NH = mh(Dir, Head),
  NPs = Points#{1 => NH},
  Fold =
    fun(P, {NP, PP}) ->
        #{P := Tail} = PP,
        NT = mt(NP, Tail),
        {NT, PP#{P => NT}}
    end,
  {NT, NPoints} = lists:foldl(Fold, {NH, NPs}, lists:seq(2,10)),
  {NPoints, Visited#{NT => true}}.
