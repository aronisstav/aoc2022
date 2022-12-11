#!/usr/bin/env escript

%% https://adventofcode.com/2022/day/5

-mode(compile).

-define(STACKS, 3).

main(Args) ->
  Input = read_input(),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~p~n", [Ans]).

read_input() ->
  Lines = read_lines([]),
  read_input(Lines, #{}, [], false).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> lists:append(lists:reverse(Acc));
    Res -> read_lines([[Res]|Acc])
  end.


read_input([], Stacks, Moves, true) ->
  {Stacks, lists:reverse(Moves)};
read_input(["\n"|R], Stacks, Moves, Status) ->
  read_input(R, Stacks, Moves, Status);
read_input([" 1 " ++ _|R], Stacks, Moves, false) ->
  read_input(R, finalize_stacks(Stacks), Moves, true);
read_input([StackLine|R], Stacks, Moves, false) ->
  read_input(R, add_stacks(StackLine, Stacks), Moves, false);
read_input([MoveLine|R], Stacks, Moves, true) ->
  read_input(R, Stacks, add_move(MoveLine, Moves), true).

finalize_stacks(Stacks) ->
  Map = fun(_, V) -> lists:reverse(V) end,
  maps:map(Map, Stacks).

add_stacks(StackLine, Stacks) ->
  add_line(" " ++ StackLine, Stacks, 1).

add_line("\n", Stacks, _) -> Stacks;
add_line([$ ,$[,C,$]|R], Stacks, N) ->
  Fun = fun(Old) -> [C|Old] end,
  NStacks = maps:update_with(N, Fun, [C], Stacks),
  add_line(R, NStacks, N + 1);
add_line("    " ++ R, Stacks, N) ->
  add_line(R, Stacks, N + 1).

add_move(MoveLine, Moves) ->
  {ok, [N, F, T], _} = io_lib:fread("move ~d from ~d to ~d", MoveLine),
  [{N,F,T}|Moves].  

solve_first(Input) ->
  solve(Input, fun lists:reverse/2).

solve({Stacks, Moves}, SFun) ->
  Fold =
    fun({N, F, T}, S) ->
        #{ F := From
         , T := To
         } = S,
        {Front, Back} = lists:split(N, From),
        S#{F => Back
          ,T => SFun(Front, To)
          }
    end,
  FS = lists:foldl(Fold, Stacks, Moves),
  Fold2 =
    fun(N, A) ->
        #{N := [F|_]} = FS,
        [F|A]
    end,
  lists:foldr(Fold2, [], lists:seq(1, maps:size(FS))).

solve_second(Input) ->
  solve(Input, fun lists:append/2).
