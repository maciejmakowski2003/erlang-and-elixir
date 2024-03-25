%%%-------------------------------------------------------------------
%%% @author macie
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. mar 2024 13:39
%%%-------------------------------------------------------------------
-module(power).
-author("macie").

-export([power/2]).

power(_, 0) ->
  1;
power(X,Y) when Y>0 ->
  X * power(X,Y-1).