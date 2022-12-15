#!/usr/bin/env escript

%% https://adventofcode.com/2022/day/15

-mode(compile).

main(Args) ->
  Input = read_list("Sensor at x=~d, y=~d: closest beacon is at x=~d, y=~d"),
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
  Target = 2000000,
  Coverage = check_line(Target, Input),
  count_empty(Target, Coverage).

check_line(Y, Input) ->
  {MinX, MaxX} = find_edge(Y, Input),
  cover_line(Y, MinX, MaxX, Input, #{}).

find_edge(Y, Input) ->
  Fold = fun(Data, Acc) -> check_edge(Data, Y, Acc) end,
  lists:foldl(Fold, {infinity, -10000000}, Input).

check_edge([SX, SY, BX, BY], Y, {Min, Max} = Limits) ->
  D = dist({SX, SY}, {BX, BY}),
  DY = dist({SX, SY}, {SX, Y}),
  XR = DY - D,
  if XR > 0 -> Limits;
     true -> {min(Min, SX + XR), max(Max, SX - XR)}
  end.

dist({A, B}, {C, D}) ->
  abs(A - C) + abs(B - D).

cover_line(_, XX, MaxX, Input, Acc) when XX > MaxX ->
  Fold = fun([_,_,X,Y], A) -> A#{{X,Y} => $B} end,
  lists:foldl(Fold, Acc, Input);
cover_line(Y, X, MaxX, Input, Acc) ->
  Pos = {X, Y},
  NAcc =
    case covered(Pos, Input) of
      true -> Acc#{Pos => $#};
      false -> Acc
    end,
  cover_line(Y, X + 1, MaxX, Input, NAcc).

covered(Pos, Input) ->
  Pred = fun(D) -> covers(Pos, D) end,
  lists:any(Pred, Input).

covers(Pos, [SX, SY, BX, BY]) ->
  D = dist({SX, SY}, {BX, BY}),
  DP = dist({SX, SY}, Pos),
  DP =< D.

count_empty(TY, Map) ->
  Fold1 =
    fun({X, Y}, _, {Min, Max} = Ls) ->
        case Y =:= TY of
          false -> Ls;
          true -> {min(Min, X), max(Max, X)}
        end
    end,
  {Min, Max} = maps:fold(Fold1, {infinity, -1000}, Map),
  Ps = lists:seq(Min, Max),
  Fold2 =
    fun(X, Acc) ->
        case maps:get({X, TY}, Map, false) of
          $# -> Acc + 1;
          _ -> Acc
        end
    end,
  lists:foldl(Fold2, 0, Ps).

solve_second(Input) ->
  search_boundaries(Input, Input).

search_boundaries([D|Rest], Input) ->
  case search_boundary(D, Input) of
    {ok, {X, Y}} -> X * 4000000 + Y;
    none -> search_boundaries(Rest, Input)
  end.

search_boundary([SX, SY, BX, BY], Input) ->
  D = dist({SX, SY}, {BX, BY}) + 1,
  Diags =
    [ {SX + D, SY, -1,  1}
    , {SX - D, SY,  1,  1}
    , {SX, SY + D,  1, -1}
    , {SX, SY - D, -1, -1}
    ],
  search_diags(Diags, D, Input).

search_diags([], _, _) -> none;
search_diags([{X, Y, DX, DY}|Rest], D, Input) ->
  case search_diag(X, Y, DX, DY, Input, D) of
    none -> search_diags(Rest, D, Input);
    {ok, _} = OK -> OK
  end.

search_diag(_, _, _, _, _, -1) -> none;
search_diag(X, Y, DX, DY, Input, D) ->
  case within_limit(X, Y) of
    true ->
      case covered({X, Y}, Input) of
        true -> search_diag(X + DX, Y + DY, DX, DY, Input, D - 1);
        false -> {ok, {X, Y}}
      end;
    false -> none
  end.

within_limit(X, Y) ->
  L = 0,
  H = 4000000,
  X >= L
    andalso X =< H
    andalso Y >= L
    andalso Y =< H.  
