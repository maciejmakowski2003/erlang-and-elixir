%%%-------------------------------------------------------------------
%%% @author macie
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. mar 2024 11:39
%%%-------------------------------------------------------------------
-module(pollution).
-author("macie").
-record(station, {name, coords, measurements = #{}}).
-record(monitor, {coords_name = #{}, stations = #{}}).
-record(measurement, {type,date}).
-export([create_monitor/0,add_station/3, add_value/5, remove_value/4,
  get_one_value/4, get_station_mean/3, get_daily_mean/3, data/0, get_location_value/4]).

if_contains_coords(Coords_Name, Coords) ->
  case maps:is_key(Coords, Coords_Name) of
    false -> {ok,""};
    true -> {error,"map contains that coordinates"}
  end.

if_contains_name(#monitor{coords_name = Coords_Name, stations = Stations}, Name, Coords) ->
  case maps:is_key(Name,Stations) of
    true -> {error,"map contains that name"};
    false -> if_contains_coords(Coords_Name, Coords)
  end.

if_contains_same_measurement(Measurements, Type, Date) ->
  case maps:is_key(#measurement{type = Type, date = Date}, Measurements) of
    false -> false;
    true ->
      maps:take(#measurement{type = Type,date = Date},Measurements)
  end.

update_measurements(New_Measurements, Name, Coords, Stations, Coords_Name) ->
  New_Station = #station{name = Name, coords = Coords, measurements = New_Measurements},
  New_Stations = maps:update(Name,New_Station,Stations),
  New_Monitor = #monitor{stations = New_Stations,coords_name = Coords_Name },
  New_Monitor.

if_station_exists({X,Y}, Coords_Name, _)->
  case maps:is_key({X,Y}, Coords_Name) of
    true -> maps:get({X,Y},Coords_Name);
    false -> false
  end;
if_station_exists(Name, _, Stations)->
  case maps:is_key(Name, Stations) of
    true -> Name;
    false -> false
  end.

create_monitor() ->
  #monitor{}.

add_station(Name, Coords, #monitor{coords_name = Coords_Name, stations = Stations}) ->
  case if_contains_name(#monitor{coords_name = Coords_Name, stations = Stations}, Name, Coords) of
    {error,Msg} -> {error,Msg};
    _ ->
      New_Coords_Name = maps:put(Coords, Name, Coords_Name),
      New_Station = #station{name = Name, coords = Coords},
      New_Stations = maps:put(Name, New_Station, Stations),
      New_Monitor = #monitor{coords_name = New_Coords_Name, stations = New_Stations},
      New_Monitor
  end.

add_value(Name_Or_Coords,Date, Type, Value, #monitor{ stations = Stations, coords_name =  Coords_Name})->
  case if_station_exists(Name_Or_Coords, Coords_Name, Stations) of
    false -> {error,"station does not exist"};
    Name ->
      #station{name = Name, coords = Coords, measurements = Measurements} = maps:get(Name,Stations),
      case if_contains_same_measurement(Measurements, Type, Date) of
        false ->
          Measurement = #measurement{type = Type, date = Date},
          New_Measurements = Measurements#{Measurement => Value},
          update_measurements(New_Measurements,Name,Coords, Stations, Coords_Name);
        _ -> {error,"same measurement exists"}
      end
  end.

remove_value(Name_Or_Coords, Date, Type, #monitor{coords_name = Coords_Name, stations = Stations}) ->
  case if_station_exists(Name_Or_Coords, Coords_Name, Stations) of
    false -> {error,"station does not exist"};
    Name ->
      #station{name = Name, coords = Coords, measurements = Measurements} = maps:get(Name,Stations),
      case if_contains_same_measurement(Measurements, Type, Date) of
        false -> {error,"measurement does not exist"};
        {_,New_Measurements} ->
          update_measurements(New_Measurements,Name,Coords, Stations, Coords_Name)
      end
  end.

get_one_value(Name_Or_Coords, Date, Type, #monitor{coords_name = Coords_Name, stations = Stations}) ->
  case if_station_exists(Name_Or_Coords, Coords_Name, Stations) of
    false -> {error,"station does not exist"};
    Name ->
      #station{name = Name, measurements = Measurements} = maps:get(Name,Stations),
      case if_contains_same_measurement(Measurements, Type, Date) of
        false -> {error,"measurement does not exist"};
        {Value,_} -> Value
      end
  end.

get_station_mean(Name_Or_Coords, Type, #monitor{coords_name = Coords_Name, stations = Stations}) ->
  case if_station_exists(Name_Or_Coords, Coords_Name, Stations) of
    false -> {error,"station does not exist"};
    Name ->
      #station{name = Name, measurements = Measurements} = maps:get(Name,Stations),
      Pred = fun(#measurement{type = Type_}, _) when Type =:= Type_ -> true; (_,_) -> false end,
      Filtered = maps:filter(Pred,Measurements),
      case maps:size(Filtered) of
        0 -> {error, "no data"};
        Length -> maps:fold(fun(_,Value, Acc) -> Value + Acc end, 0, Filtered)/Length
      end
  end.

get_daily_mean(Type, Date, #monitor{stations = Stations}) ->
  Fun1 = fun(_,#station{measurements = Measurements}, Acc) -> Acc ++ maps:to_list(Measurements) end,
  Measurements = maps:fold(Fun1,[],Stations),
  Fun2 = fun({#measurement{type = Type_, date = {Date_,_}},Value}, Acc)
    when Date_ =:= Date andalso Type_ =:= Type -> Acc ++ [Value]; (_,Acc) -> Acc end,
  Filtered = lists:foldl(Fun2,[],Measurements),
  case length(Filtered) of
    0 -> {error, "no data"};
    Length -> lists:sum(Filtered)/Length
  end.


split_measurements(Measurements, Type, Date, Coords, Measurement_Coords) ->
  Fun = fun({_, Type_, Date_}, Value, Acc)
    when Type_ =:= Type andalso Date_ =:= Date -> Acc ++ [{Value, get_distance(Coords, Measurement_Coords)}];
    (_,_,Acc) -> Acc end,
  maps:fold(Fun,[],Measurements).

get_distance({X1,Y1}, {X2,Y2}) ->
  math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

get_weighted_mean(Measurements)->
  Fun = fun({Value,_},[Acc,Counter]) -> [Acc+Value*Counter,Counter-1] end,
  [Sum,_] = lists:foldl(Fun,[0,length(Measurements)],Measurements),
  (2*Sum/(length(Measurements)*(length(Measurements)+1))).

get_location_value(Coords, Type, Date, #monitor{stations = Stations})->
  Fun1 = fun(_,#station{coords = Measurement_Coords, measurements = Measurements}, Acc)
    -> Acc ++ split_measurements(Measurements, Type, Date, Coords, Measurement_Coords) end,
  Measurements = maps:fold(Fun1,[],Stations),
  if
    length(Measurements) < 3 -> {error, "no enough data"};
    true -> get_weighted_mean(lists:sublist(lists:keysort(2,Measurements),3))
  end.


data() ->
  M = pollution:add_station("Stacja 3", {3,3}, pollution:add_station("Stacja 2", {2,2}, pollution:add_station("Stacja 1", {1,1},pollution:add_station("Stacja 4", {4,4}, pollution:create_monitor())))),
  M1 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10, M),
  M2 = pollution:add_value("Stacja 2", {{2023,3,27},{11,16,10}}, "PM10", 20, M1),
  M3 = pollution:add_value("Stacja 3", {{2023,3,27},{11,16,10}}, "PM10", 30, M2),
  M4 = pollution:add_value("Stacja 4", {{2023,3,27},{11,16,10}}, "PM10", 40, M3),
  M5 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM2,5", 10, M4),
  M5.

