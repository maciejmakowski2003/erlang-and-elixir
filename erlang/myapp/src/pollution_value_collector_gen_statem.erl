%%%-------------------------------------------------------------------
%%% @author macie
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2024 15:33
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).
-author("macie").

-behaviour(gen_statem).

%% API
-export([start_link/0, stop/0, set_station/1, add_value/3, store_data/0]).

%% gen_statem callbacks
-export([init/1, handle_event/4, callback_mode/0]).

-define(SERVER, ?MODULE).

-record(state, {station = undefined, values = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_statem:stop(?MODULE).

set_station(Name_Or_Coords) ->
  gen_statem:call(?MODULE, {set_station, Name_Or_Coords}).

add_value(Date, Type, Value) ->
  gen_statem:call(?MODULE, {add_value, Date, Type, Value}).

store_data() ->
  gen_statem:cast(?MODULE, store_data).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([]) ->
  {ok, waiting_for_station, #state{}}.

callback_mode() ->
  handle_event_function.

handle_event({call, From}, {set_station, Name_Or_Coords}, waiting_for_station, State) ->
  {next_state, collecting_values, State#state{station = Name_Or_Coords}, [{reply, From, ok}]};

handle_event({call, From}, {add_value, Date, Type, Value}, collecting_values, State=#state{values = Values}) ->
  {keep_state, State#state{values = [{Date, Type, Value} | Values]}, [{reply, From, ok}]};

handle_event({call,From}, {store_data}, collecting_values, State=#state{station = Station, values = Values}) ->
  AddedValues = [pollution_gen_server:addValue(Station, Date, Type, Value) || {Date, Type, Value} <- Values],
  {next_state, waiting_for_station, State#state{station = undefined, values = []}, [{reply, From, AddedValues}]};

%%% catch-all clause to handle unexpected events
handle_event(_Event, _From, _StateName, State) ->
  {keep_state, State}.