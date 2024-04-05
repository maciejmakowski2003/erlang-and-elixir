%%%-------------------------------------------------------------------
%%% @author macie
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. mar 2024 10:17
%%%-------------------------------------------------------------------
-module(myList).
-author("macie").

-export([contains/2, duplicateElements/1, sumFloats/2]).

contains([], _) ->
  false;
contains([H|T], X) ->
  if
    H == X -> true;
    true -> contains(T, X)
  end.

duplicateElements([]) ->
  [];
duplicateElements([H|T]) ->
  [H, H | duplicateElements(T)].

sumFloats([], Acc) ->
  Acc;
sumFloats([H|T], Acc) ->
  case is_float(H) of
    true -> sumFloats(T, H + Acc);
    false -> sumFloats(T, Acc)
  end.
