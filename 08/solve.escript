#!/usr/bin/env escript

%% https://adventofcode.com/2022/day/8

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
    eof -> to_map(lists:reverse(Acc));
    Res ->
      [_|R] = lists:reverse(Res),
      read_lines([lists:reverse(R)|Acc])
  end.

to_map(Lines) ->
  Fold =
    fun(Line, {Y, _, Map}) ->
        IFold =
          fun(S, {X, IMap}) ->
              {X + 1, IMap#{{X, Y} => S - $0}}
          end,
        {L, OMap} = lists:foldl(IFold, {1, Map}, Line),
        {Y + 1, L, OMap}
    end,
  {YM, XM, OMap} = lists:foldl(Fold, {1, 0, #{}}, Lines),
  {XM - 1, YM - 1, OMap}.

solve_first({_, _, Grid} = Map) ->
  Status0 = clean_map(Grid, false),
  Status1 = check_ud(Status0, Map),
  Status2 = check_du(Status1, Map),
  Status3 = check_lr(Status2, Map),
  Status4 = check_rl(Status3, Map),
  count_visible(Status4).

clean_map(Map, Value) ->
  Fun = fun(_, _) -> Value end,
  maps:map(Fun, Map).

check_ud(Status, {MX, MY, Grid}) ->
  Ys = lists:reverse([break|lists:seq(MY, 1, -1)]),
  Xs = lists:seq(1, MX),
  Order = [{X, Y} || X <- Xs, Y <- Ys],
  check_order(Order, Grid, Status).

check_du(Status, {MX, MY, Grid}) ->
  Ys = lists:reverse([break|lists:seq(1, MY)]),
  Xs = lists:seq(1, MX),
  Order = [{X, Y} || X <- Xs, Y <- Ys],
  check_order(Order, Grid, Status).

check_lr(Status, {MX, MY, Grid}) ->
  Xs = lists:reverse([break|lists:seq(MX, 1, -1)]),
  Ys = lists:seq(1, MY),
  Order = [{X, Y} || Y <- Ys, X <- Xs],
  check_order(Order, Grid, Status).

check_rl(Status, {MX, MY, Grid}) ->
  Xs = lists:reverse([break|lists:seq(1, MX)]),
  Ys = lists:seq(1, MY),
  Order = [{X, Y} || Y <- Ys, X <- Xs],
  check_order(Order, Grid, Status).

check_order(Order, Grid, Status) ->
  Fold =
    fun(Pos, {Height, Acc}) ->
        case Pos of
          {break, _} -> {-1, Acc};
          {_, break} -> {-1, Acc};
          {X, Y} ->
            #{{X,Y} := H} = Grid,
            case H > Height of
              true -> {H, Acc#{Pos => true}};
              false -> {Height, Acc}
            end
        end
    end,
  {_, New} = lists:foldl(Fold, {-1, Status}, Order),
  New.

count_visible(Grid) ->
  Fold =
    fun(_, V, Acc) ->
        case V of
          true -> Acc + 1;
          false -> Acc
        end
    end,
  maps:fold(Fold, 0, Grid).

solve_second({_, _, Grid}) ->
  Fold = fun(Pos, H, Acc) -> max(Acc, scenic(Pos, H, Grid)) end,
  maps:fold(Fold, 0, Grid).

scenic(Pos, H, Grid) ->
  scenic(Pos, H, { 0,-1}, Grid) *
  scenic(Pos, H, { 0, 1}, Grid) *
  scenic(Pos, H, {-1, 0}, Grid) *
  scenic(Pos, H, { 1, 0}, Grid).

scenic(Pos, H, Inc, Grid) ->
  scenic(add(Pos, Inc), H, Inc, Grid, 0).

add({X, Y}, {A, B}) -> {X + A, Y + B}.

scenic(Pos, H, Inc, Grid, Acc) ->
  case maps:find(Pos, Grid) of
    error -> Acc;
    {ok, L} when L < H ->
      scenic(add(Pos, Inc), H, Inc, Grid, Acc + 1);
    _ -> Acc + 1
  end.
