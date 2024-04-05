%%%-------------------------------------------------------------------
%%% @author macie
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2024 11:54
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("macie").

-export([start/0,stop/0, init/0]).
-export([add_station/2, add_value/4, add_data/0]).
-export([remove_value/3]).
-export([get_one_value/3, get_location_value/3, get_daily_mean/2, get_station_mean/2, get_monitor/0]).
-import(pollution,[create_monitor/0]).

start()->
  register(server, spawn(?MODULE, init,[])).

init()->
  Monitor = pollution:create_monitor(),
  loop(Monitor).

stop()->
  server ! {stop,self()}.

handle_request(Monitor, New_Monitor, PID)->
  case New_Monitor of
    {error,Msg} ->
      PID ! {reply, {error,Msg}},
      loop(Monitor);
    _ ->
      PID ! {reply, ok},
      loop(New_Monitor)
  end.

handle_get_request(Monitor, Value, PID)->
  case Value of
    {error,Msg} ->
      PID ! {reply, {error,Msg}};
    _ ->
      PID ! {reply, Value}
  end,
  loop(Monitor).

loop(Monitor)->
  receive
    {post, PID, station,{Name, Coords}} ->
      New_Monitor = pollution:add_station(Name, Coords, Monitor),
      handle_request(Monitor, New_Monitor, PID);
    {post, PID, value, {Name_Or_Coords,Date, Type, Value}} ->
      New_Monitor = pollution:add_value(Name_Or_Coords, Date, Type, Value, Monitor),
      handle_request(Monitor, New_Monitor, PID);
    {post, PID, sample_monitor} ->
      New_Monitor = pollution:data(),
      handle_request(Monitor, New_Monitor, PID);
    {delete, PID, value, {Name_Or_Coords, Date, Type}} ->
      New_Monitor = pollution:remove_value(Name_Or_Coords, Date, Type, Monitor),
      handle_request(Monitor, New_Monitor, PID);
    {get, PID, value, {Name_Or_Coords, Date, Type}} ->
      Value = pollution:get_one_value(Name_Or_Coords, Date, Type, Monitor),
      handle_get_request(Monitor, Value, PID);
    {get, PID, station_mean, {Name_Or_Coords, Type}} ->
      Value = pollution:get_station_mean(Name_Or_Coords, Type, Monitor),
      handle_get_request(Monitor, Value, PID);
    {get, PID, daily_mean, {Type, Date}} ->
      Value = pollution:get_daily_mean(Type, Date, Monitor),
      handle_get_request(Monitor, Value, PID);
    {get, PID, location_value, {Coords, Type, Date}} ->
      Value = pollution:get_location_value(Coords, Type, Date, Monitor),
      handle_get_request(Monitor, Value, PID);
    {get, PID, monitor} ->
      PID ! {reply,Monitor},
      loop(Monitor);
    stop ->
      stop
  end.

request(Type, Name, Args) ->
  server ! {Type, self(), Name, Args},
  receive
    {reply, {error, Msg}} -> Msg;
    {reply, Reply} -> Reply
  end.

request(Type, Name) ->
  server ! {Type, self(), Name},
  receive
    {reply, {error, Msg}} -> Msg;
    {reply, Reply} -> Reply
  end.

add_station(Name, Coords) -> request(post, station, {Name, Coords}).
add_value(Name_Or_Coords,Date, Type, Value) -> request(post, value, {Name_Or_Coords,Date, Type, Value}).
add_data() -> request(post, sample_monitor).
remove_value(Name_Or_Coords, Date, Type) -> request(delete,value,{Name_Or_Coords, Date, Type}).
get_one_value(Name_Or_Coords, Date, Type) -> request(get,value,{Name_Or_Coords, Date, Type}).
get_station_mean(Name_Or_Coords, Type) -> request(get,station_mean,{Name_Or_Coords, Type}).
get_daily_mean(Type, Date) -> request(get, daily_mean, {Type, Date}).
get_location_value(Coords, Type, Date) -> request(get, location_value, {Coords, Type, Date}).
get_monitor() -> request(get,monitor).