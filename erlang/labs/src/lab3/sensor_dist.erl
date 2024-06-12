%%%-------------------------------------------------------------------
%%% @author macie
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2024 10:22
%%%-------------------------------------------------------------------
-module(sensor_dist).
-author("macie").
-export([get_rand/1, find_closest/2, find_closest_parallel/2]).

get_rand(N) ->
  People = [ {rand:uniform(10000), rand:uniform(10000)} || _ <- lists:seq(1, N)],
  Sensors = [ {rand:uniform(10000), rand:uniform(10000)} || _ <- lists:seq(1, N)],
  [People, Sensors].

dist({X1, Y1}, {X2, Y2}) ->
  math:sqrt((X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2)).

find_for_person(PersonLocation, SensorsLocations) ->
  {Dist, Location} = lists:min([{dist(PersonLocation,SensorLocation),SensorLocation} || SensorLocation <- SensorsLocations]),
  {Dist, {PersonLocation, Location}}.

find_closest(PeopleLocations, SensorsLocations) ->
  lists:min([find_for_person(Person, SensorsLocations) || Person <- PeopleLocations]).

find_for_person(PersonLocation, SensorsLocations, ParentPID) ->
  ParentPID ! find_for_person(PersonLocation, SensorsLocations).

find_closest_parallel(PeopleLocations, SensorsLocations) ->
  ParentPID = self(),
  [spawn(fun() -> find_for_person(Person, SensorsLocations, ParentPID) end) || Person <- PeopleLocations],
  Results = receive_results(length(PeopleLocations), []),
  lists:min(Results).

receive_results(0,Acc) -> Acc;
receive_results(N,Acc) ->
  receive
    Result ->
      receive_results(N-1,[Result | Acc])
  end .