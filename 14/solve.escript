#!/usr/bin/env escript

%% https://adventofcode.com/2022/day/14

-mode(compile).

main(Args) ->
  Input = read_lines(),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~p~n", [Ans]).

read_lines() -> read_lines(#{}).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> add_edge(Acc);
    Res ->
      Tokens = string:tokens(Res, "\n ->"),
      read_lines(add_line(Tokens, Acc))
  end.

add_line(Tokens, Acc) ->
  [H|T] = [pair(T) || T <- Tokens],
  add_segment(H, T, Acc).

pair(String) ->
  {ok, [A,B], _} = io_lib:fread("~d,~d",String),
  {A,B}.

add_segment(_, [], Acc) -> Acc;
add_segment(From, [To|Rest], Acc) ->
  {X1, Y1} = From,
  {X2, Y2} = To,
  Fold = fun(Pos, A) -> A#{Pos => $#} end,
  Poss = [{X, Y} || X <- line(X1, X2), Y <- line(Y1, Y2)],
  add_segment(To, Rest, lists:foldl(Fold, Acc, Poss)).

line(X1, X2) ->
  lists:seq(min(X1, X2), max(X1, X2)).

add_edge(Map) ->
  Fold =
    fun({X, Y}, _, {{LX, LY}, {HX, HY}}) ->
        {{min(X, LX), min(Y, LY)}, {max(X, HX), max(Y, HY)}}
    end,
  {Min, Max} = maps:fold(Fold, {{500, 0}, {500, 0}}, Map),
  Map#{min => Min, max => Max}.

solve_first(Map) ->
  sand(Map#{floor => off}).

sand(Map) ->
  case sand({500, 0}, Map) of
    {ok, NM} ->
      %%print(NM),
      sand(NM);
    error ->
      print(Map),
      count_sand(Map)
  end.

sand({_, Y}, #{floor := off, max := {_, HY}}) when Y > HY -> error;
sand({_, Y} = Pos, #{max := {_, HY}} = Map) when Y - HY == 1 ->
  {ok, Map#{Pos => $o}};
sand({X, Y} = Pos, Map) ->
  case maps:get(Pos, Map, false) =:= false of
    false -> error;
    true ->
      case maps:get({X, Y + 1}, Map, false) of
        false -> sand({X, Y + 1}, Map);
        _ ->
          case maps:get({X - 1, Y + 1}, Map, false) of
            false -> sand({X - 1, Y + 1}, Map);
            _ ->
              case maps:get({X + 1, Y + 1}, Map, false) of
                false -> sand({X + 1, Y + 1}, Map);
                _ -> {ok, Map#{Pos => $o}}
              end
          end
      end
  end.

count_sand(Map) ->
  Fold =
    fun(_, C, Acc) ->
        case C =:= $o of
          true -> Acc + 1;
          false -> Acc
        end
    end,
  maps:fold(Fold, 0, Map).

print(Map) ->
  #{ min := {LX, LY}
   , max := {HX, HY}
   } = Map,
  Foreach =
    fun(Y) ->
        Foreach2 =
          fun(X) ->
              case maps:get({X, Y}, Map, false) of
                false -> io:format(" ");
                C -> io:format("~c", [C])
              end
          end,
          lists:foreach(Foreach2, lists:seq(LX, HX)),
        io:format("~n")
    end,
  lists:foreach(Foreach, lists:seq(LY, HY)),
  0.

solve_second(Map) ->
  sand(Map#{floor => on}).
