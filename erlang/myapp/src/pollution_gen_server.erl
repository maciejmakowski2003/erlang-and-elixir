%%%-------------------------------------------------------------------
%%% @author macie
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([start_link/0, addStation/2, addValue/4,
  getOneValue/3, getStationMean/2, getDailyMean/2,
  getLocationValue/3, removeValue/3, crash/0]).
-export([init/1, handle_call/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

%%%===================================================================
%%% observer:start(). - starts application observer
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, pollution:create_monitor()}.

addStation(Name, Coords) ->
  gen_server:call(?SERVER, {addStation, Name, Coords}).

addValue(Name_Or_Coords, Date, Type, Value) ->
  gen_server:call(?SERVER, {addValue, Name_Or_Coords, Date, Type, Value}).

getOneValue(Name_Or_Coords, Date, Type) ->
  gen_server:call(?SERVER, {getOneValue, Name_Or_Coords, Date, Type}).

removeValue(Name_Or_Coords, Date, Type) ->
  gen_server:call(?SERVER, {removeValue, Name_Or_Coords, Date, Type}).

getStationMean(Name_Or_Coords, Type) ->
  gen_server:call(?SERVER, {getStationMean, Name_Or_Coords, Type}).

getDailyMean(Type, Date) ->
  gen_server:call(?SERVER, {getDailyMean, Type, Date}).

getLocationValue(Coords, Type, Date) ->
  gen_server:call(?SERVER, {getLocationValue, Coords, Type, Date}).

crash() ->
  gen_server:call(?SERVER, crash).

handle_call({addStation, Name, Coords}, _From, State) ->
  case pollution:add_station(Name, Coords, State) of
    {error, Msg} -> {reply, {error, Msg}, State};
    New_State -> {reply, ok, New_State}
  end;
handle_call({addValue, Name_Or_Coords, Date, Type, Value}, _From, State) ->
  case pollution:add_value(Name_Or_Coords, Date, Type, Value, State) of
    {error, Msg} -> {reply, {error, Msg}, State};
    New_State -> {reply, ok, New_State}
  end;
handle_call({getOneValue, Name_Or_Coords, Date, Type}, _From, State) ->
  case pollution:get_one_value(Name_Or_Coords, Date, Type, State) of
    {error, Msg} -> {reply, {error, Msg}, State};
    Value -> {reply, Value, State}
  end;
handle_call({removeValue, Name_Or_Coords, Date, Type}, _From, State) ->
  case pollution:remove_value(Name_Or_Coords, Date, Type, State) of
    {error, Msg} -> {reply, {error, Msg}, State};
    New_State -> {reply, ok, New_State}
  end;
handle_call({getStationMean, Name_Or_Coords, Type}, _From, State) ->
  case pollution:get_station_mean(Name_Or_Coords, Type, State) of
    {error, Msg} -> {reply, {error, Msg}, State};
    Mean -> {reply, Mean, State}
  end;
handle_call({getDailyMean, Type, Date}, _From, State) ->
  case pollution:get_daily_mean(Type, Date, State) of
    {error, Msg} -> {reply, {error, Msg}, State};
    Mean -> {reply, Mean, State}
  end;
handle_call({getLocationValue, Coords, Type, Date}, _From, State) ->
  case pollution:get_location_value(Coords, Type, Date, State) of
    {error, Msg} -> {reply, {error, Msg}, State};
    Value -> {reply, Value, State}
  end.