#!/usr/bin/env escript

%% https://adventofcode.com/2022/day/13

-mode(compile).

main(Args) ->
  Input = read_lines(),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~w~n", [Ans]).

read_lines() -> read_lines([]).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> parse(lists:reverse(Acc), 1, []);
    Res ->
      [_|R] = lists:reverse(Res),
      case R =:= "" of
        true -> read_lines(Acc);
        false -> read_lines([lists:reverse([$.|R])|Acc])
      end
  end.

parse([], _, Acc) ->
  lists:reverse(Acc);
parse([L1, L2|Rest], N, Acc) ->
  [P1, P2] = [parse_one(L) || L <- [L1, L2]],
  parse(Rest, N + 1, [{N, P1, P2}|Acc]).

parse_one(Line) ->
  {ok, To, _} = erl_scan:string(Line),
  {ok, Te} = erl_parse:parse_term(To),
  Te.

solve_first(Input) ->
  Fold =
    fun({I, A, B}, Acc) ->
        O = ordered(A, B),
        io:format("~w~n~w~n~w~n~n",[A, B, O]),
        case O of
          true -> Acc + I;
          _ -> Acc
        end
    end,
  lists:foldl(Fold, 0, Input).

ordered(A, B) when is_integer(A), is_integer(B) ->
  if A < B -> true;
     A > B -> false;
     true -> continue
  end;
ordered([A|RA], [B|RB]) ->
  case ordered(A, B) of
    continue -> ordered(RA, RB);
    Else -> Else
  end;
ordered([], [_|_]) -> true;
ordered([_|_], []) -> false;
ordered([], []) -> continue; %% Tricky!
ordered(A, B) ->
  case is_integer(A) of
    true -> ordered([A], B);
    false -> ordered(A, [B])
  end.            

solve_second(Input) ->
  Fold = fun({_, A, B}, Acc) -> [A,B|Acc] end,
  All = [[[2]], [[6]] | lists:foldl(Fold, [], Input)],
  Sort = lists:sort(fun ordered/2, All),
  Fold2 =
    fun(X, {Done, I, R} = Acc) ->
        case {Done, X} of
          {true, _} -> Acc;
          {_, [[2]]} -> {Done, I + 1, I};
          {_, [[6]]} -> {true, I + 1, R * I};
          {_, _} -> {Done, I + 1, R}
        end
    end,
  {true, _, X} = lists:foldl(Fold2, {false, 1, 0}, Sort),
  X.
