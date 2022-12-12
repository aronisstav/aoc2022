#!/usr/bin/env escript

%% https://adventofcode.com/2022/day/12

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
              NIMap =
                case S of
                  $S ->
                    IMap#{{X, Y} => $a - $a, start => {X, Y}};
                  $E ->
                    IMap#{{X, Y} => $z - $a, fin => {X, Y}};
                  _ ->
                    IMap#{{X, Y} => S - $a}
                end,
              {X + 1, NIMap}
          end,
        {L, OMap} = lists:foldl(IFold, {1, Map}, Line),
        {Y + 1, L, OMap}
    end,
  {YM, XM, OMap} = lists:foldl(Fold, {1, 0, #{}}, Lines),
  {XM - 1, YM - 1, OMap}.

solve_first({_, _, Grid}) ->
  #{ start := S
   , fin := F
   } = Grid,
  Q = gb_sets:from_list([{0, S}]),
  search(Q, F, Grid, #{}).

search(Queue, Goal, Grid, Visited) ->
  {{S, Pos}, NQ} = gb_sets:take_smallest(Queue),
  Cur = maps:get(Pos, Visited, infinity),
  case S > Cur of
    true ->
      search(NQ, Goal, Grid, Visited);
    false ->
      case Pos =:= Goal of
        true -> S;
        false ->
          NV = Visited#{Pos => S},
          FQ = add_neighs(Pos, S, Grid, NQ),
          search(FQ, Goal, Grid, NV)
      end
  end.

add_neighs({X, Y} = Pos, S, Grid, Q) ->
  H = maps:get(Pos, Grid),
  Neighs = [{X + A, Y + B} || {A, B} <- [{1,0},{-1,0},{0,1},{0,-1}]],
  NHs = [{maps:get(PP, Grid, 100), PP} || PP <- Neighs],
  Legit = [{S + 1, PP} || {HH, PP} <- NHs, HH - H < 2],
  Fold = fun(N, Acc) -> gb_sets:add_element(N, Acc) end,
  lists:foldl(Fold, Q, Legit).                 

solve_second({_, _, Grid}) ->
  #{ fin := F
   } = Grid,
  Q = gb_sets:from_list([{0, F}]),
  Final = search2(Q, Grid, #{}),
  smallest_a(Grid, Final).

search2(Queue, Grid, Visited) ->
  case gb_sets:is_empty(Queue) of
    true -> Visited;
    false ->
      {{S, Pos}, NQ} = gb_sets:take_smallest(Queue),
      Cur = maps:get(Pos, Visited, infinity),
      case S > Cur of
        true ->
          search2(NQ, Grid, Visited);
        false ->
          NV = Visited#{Pos => S},
          FQ = add_neighs2(Pos, S, Grid, NQ),
          search2(FQ, Grid, NV)
      end
  end.

add_neighs2({X, Y} = Pos, S, Grid, Q) ->
  H = maps:get(Pos, Grid),
  Neighs = [{X + A, Y + B} || {A, B} <- [{1,0},{-1,0},{0,1},{0,-1}]],
  NHs = [{maps:get(PP, Grid, 100), PP} || PP <- Neighs],
  Legit = [{S + 1, PP} || {HH, PP} <- NHs, H - HH < 2, HH =/= 100],
  Fold = fun(N, Acc) -> gb_sets:add_element(N, Acc) end,
  lists:foldl(Fold, Q, Legit).

smallest_a(Grid, Visited) ->                
  Fold =
    fun(K, V, Acc) ->
        case V =:= 0 of
          false -> Acc;
          true ->
            case maps:get(K, Visited, infinity) of
              infinity -> Acc;
              S -> min(S, Acc)
            end
        end
    end,
  maps:fold(Fold, 1000, Grid).
