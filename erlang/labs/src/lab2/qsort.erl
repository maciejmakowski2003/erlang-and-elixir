%%%-------------------------------------------------------------------
%%% @author macie
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. mar 2024 15:44
%%%-------------------------------------------------------------------
-module(qsort).
-author("macie").
-export([qs/1, random_elems/3, compare_speeds/3]).

less_than(List, Arg) ->
  [X || X <- List, X < Arg].

grt_eq_than(List, Arg) ->
  [X || X <- List, X >= Arg].

qs([]) ->
  [];
qs([Pivot | Tail]) ->
  qs(less_than(Tail, Pivot)) ++ [Pivot] ++ qs(grt_eq_than(Tail, Pivot)).

random_elems(N, Min, Max) ->
  [rand:uniform(Max-Min) + Min || _ <- lists:seq(1,N)].

compare_speeds(List, Fun1, Fun2) ->
  {Time1, _Result1} = timer:tc(Fun1, [List]),
  {Time2, _Result2} = timer:tc(Fun2, [List]),
  io:format("Czas wykonania dla qsort:qs/1 : ~p mikrosekund.~n", [Time1]),
  io:format("Czas wykonania dla lists:sort/1 : ~p mikrosekund.~n", [Time2]).


