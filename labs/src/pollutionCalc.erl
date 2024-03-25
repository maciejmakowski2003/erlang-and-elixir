%%%-------------------------------------------------------------------
%%% @author macie
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. mar 2024 10:40
%%%-------------------------------------------------------------------
-module(pollutionCalc).
-author("macie").
-export([get_data/0, number_of_readings/2, calculate_max/2, calculate_mean/2, calculate_mean2/2]).

get_data() ->
  [
    {"stacja1", {2024, 3, 4}, {13, 39, 0}, [{"PM2.5",15},{"PM10",25},{"wiatr",30}]},
    {"stacja2", {2024, 3, 4}, {13, 39, 0}, [{"PM2.5",10},{"PM10",22},{"temperatura",13}]},
    {"stacja3", {2024, 3, 4}, {13, 39, 0}, [{"PM2.5",11},{"PM10",20},{"ciśnienie",1019}]},
    {"stacja4", {2024, 4, 4}, {13, 39, 0}, [{"PM2.5",11},{"PM10",20},{"ciśnienie",1019}]}
  ].

number_of_readings(Readings, Date) ->
  f_number_of_readings(Readings, Date, 0).

f_number_of_readings([{_, Date, _, _}|Readings], Date, Acc) ->
  f_number_of_readings(Readings, Date, Acc + 1);
f_number_of_readings([_|Readings], Date, Acc) ->
  f_number_of_readings(Readings, Date, Acc);
f_number_of_readings([], _, Acc) ->
  Acc.

calculate_max(Readings, Type) ->
  case iterate_readings_max(Readings, Type, -10000000) of
    -10000000 -> "No readings of this type";
    Max -> Max
  end.

iterate_readings_max([{_, _, _, Measurements}|Readings], Type, Acc) ->
  iterate_readings_max(Readings,Type,iterate_measurements_max(Measurements, Type, Acc));
iterate_readings_max([], _, Acc) ->
  Acc.

iterate_measurements_max([{Type, Value}|Measurements], Type, Acc) ->
  iterate_measurements_max(Measurements, Type, max(Value, Acc));
iterate_measurements_max([_|Measurements], Type, Acc) ->
  iterate_measurements_max(Measurements, Type, Acc);
iterate_measurements_max([], _, Acc) ->
  Acc.

calculate_mean(Readings, Type) ->
  case iterate_readings_mean(Readings, Type, {0, 0}) of
    {_, 0} -> "No readings of this type";
    {Sum, Counter} -> Sum / Counter
  end.

iterate_readings_mean([{_, _, _, Measurements}|Readings], Type, {Acc, Counter}) ->
  iterate_readings_mean(Readings, Type, iterate_measurements_mean(Measurements, Type, {Acc, Counter}));
iterate_readings_mean([], _, {Acc, Counter}) ->
  {Acc, Counter}.

iterate_measurements_mean([{Type, Value}|Measurements], Type, {Acc, Counter}) ->
  iterate_measurements_mean(Measurements, Type, {Value + Acc, Counter + 1});
iterate_measurements_mean([_|Measurements], Type, {Acc, Counter}) ->
  iterate_measurements_mean(Measurements, Type, {Acc, Counter});
iterate_measurements_mean([], _, {Acc, Counter}) ->
  {Acc, Counter}.

calculate_mean2(Readings,Type) ->
  Fun1 = fun({_, _, _, Measurements}) -> Measurements end,
  Fun3 = fun(X,Y) -> [Value_ || {Type_,Value_}<-X, Type_ =:= Type] ++ Y end,
  Measurements = lists:foldl(Fun3,[],lists:map(Fun1,Readings)),
  Sum = lists:sum(Measurements),
  Length = length(Measurements),
  Sum/Length.