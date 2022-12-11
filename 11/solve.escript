#!/usr/bin/env escript

%% https://adventofcode.com/2022/day/11

-mode(compile).

main(Args) ->
  Input = read_input(#{}),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~p~n", [Ans]).

read_input(Acc) ->
  case io:fread("", "Monkey ~d:") of
    {ok, [N]} ->
      Monkey = read_monkey(),
      read_input(Acc#{N => Monkey#{id => N}});
    eof -> Acc
  end.

read_monkey() ->
  Items = read_items(),
  Op = read_op(),
  Test = read_test(),
  TPass = read_pass(),
  FPass = read_pass(),
  #{ items => Items
   , op    => Op
   , test  => Test
   , true  => TPass
   , false => FPass
   , pass  => 0
   }.

read_items() ->
  [_,_|Tokens] = get_tokens(),
  queue:from_list([list_to_integer(N) || N <- Tokens]).

get_tokens() ->
  Line = io:get_line(""),
  [_|R] = lists:reverse(Line),
  SNums = lists:reverse(R),
  string:tokens(SNums, ", ").

read_op() ->
  [_,_,_,Op1,Op,Op2] = get_tokens(),
  F =
    case Op of
      "+" -> fun erlang:'+'/2;
      "*" -> fun erlang:'*'/2
    end,
  fun(V) ->
      F(
        case Op1 =:= "old" of
          true -> V;
          false -> list_to_integer(Op1)
        end,
        case Op2 =:= "old" of
          true -> V;
          false -> list_to_integer(Op2)
        end)
  end.

read_test() ->
  [_,_,_,N] = get_tokens(),
  list_to_integer(N).

read_pass() ->
  [_,_,_,_,_,N] = get_tokens(),
  list_to_integer(N).

solve_first(Input) ->
  play(20, Input#{red => fun(X) -> X div 3  end}).

play(0, State) ->
  score(State);
play(N, State) ->
  Fold = fun(I, Acc) -> play_one(I, Acc) end,
  NState = lists:foldl(Fold, State, lists:seq(0, maps:size(State) - 2)),
  play(N - 1, NState).

play_one(M, State) ->
  #{M := #{items := Items} = Monkey} = State,
  empty_queue(Items, Monkey#{items => queue:new()}, State).

empty_queue(Queue, M, #{red := Red} = State) ->
  {Out, NQueue} = queue:out(Queue),
  case Out of
    empty ->
      #{id := Id} = M,
      State#{Id => M};
    {value, Item} ->
      #{ op    := Op
       , id    := _Id
       , test  := Test
       , true  := TPass
       , false := FPass
       , pass  := Pass
       } = M,
      NItem = Red(Op(Item)),
      {NState, _To} =
        case NItem rem Test =:= 0 of
          true ->
            #{TPass := #{items := TI} = TM} = State,
            {State#{TPass => TM#{items => queue:in(NItem, TI)}}, TPass};
          false ->
            #{FPass := #{items := FI} = FM} = State,
            {State#{FPass => FM#{items => queue:in(NItem, FI)}}, FPass}
        end,
      empty_queue(NQueue, M#{pass => Pass + 1}, NState)
  end.

score(State) ->
  Fold =
    fun(I, Acc) ->
        #{I := #{pass := P}} = State,
        [P|Acc]
    end,
  Ps = lists:foldl(Fold, [], lists:seq(0, maps:size(State) - 2)),
  [A,B|_] = lists:reverse(lists:sort(Ps)),
  A * B.

solve_second(Input) ->
  Fold = fun(_, #{test := Test}, Acc) -> Acc * Test end,
  Div = maps:fold(Fold, 1, Input),
  play(10000, Input#{red => fun(X) -> X rem Div end}).
