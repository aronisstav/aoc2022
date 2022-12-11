#!/usr/bin/env escript

%% https://adventofcode.com/2022/day/7

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

solve_first([_|Input]) ->
  Tree = build_tree(Input, #{}, []),
  small_dir_sum(Tree).

build_tree([], Current, []) ->
  #{"/" => {dir, dir_size(Current), Current}};
build_tree([], Current, [{What, Old}|Stack]) ->
  build_tree([], Old#{What => {dir, dir_size(Current), Current}}, Stack);
build_tree([Line|L], Current, Stack) ->
  {Next, New} =
    case Line of
      "$ ls" ->
        {Current, Stack};
      "$ cd .." ->
        [{What, Old}|Rest] = Stack,
        {Old#{What => {dir, dir_size(Current), Current}}, Rest};
      "$ cd " ++ Name ->
        {#{}, [{Name, Current}|Stack]};
      "dir " ++ Name ->
        {Current#{Name => {dir, unknown, #{}}}, Stack};
      Else ->
        {ok, [Size, Name], _} = io_lib:fread("~d ~s",Else),
        {Current#{Name => {file, Size, none}}, Stack}
    end,
  build_tree(L, Next, New).

dir_size(Map) ->
  Fold = fun(_, {_, S, _}, Acc) -> S + Acc end,
  maps:fold(Fold, 0, Map).        

small_dir_sum(Tree) ->
  Fold =
    fun(_, {Type, Size, Rest}, Acc) ->
        case Type of
          file -> Acc;
          dir ->
            RestS = small_dir_sum(Rest),
            case Size < 100000 of
              true -> Acc + Size + RestS;
              false -> Acc + RestS
            end
        end
    end,
  maps:fold(Fold, 0, Tree).            

solve_second(Input) ->
  #{"/" := {dir, Total, Dir}} = build_tree(Input, #{}, []),
  Goal = Total - 40000000,
  smallest_gone(Goal, Dir, Total).

smallest_gone(Goal, Tree, InBest) ->
  Fold =
    fun(_, {Type, Size, Rest}, Best) ->
        case Type of
          file -> Best;
          dir ->
            NewBest = smallest_gone(Goal, Rest, Best),
            case Size > Goal of
              true -> min(NewBest, Size);
              false -> NewBest
            end
        end
    end,
  maps:fold(Fold, InBest, Tree).            
