%%%-------------------------------------------------------------------
%%% @author macie
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2024 09:06
%%%-------------------------------------------------------------------
-module(pingpong).
-author("macie").

%% API
-export([start/0, stop/0, play/1]).
-export([ping_init/0, pong_loop/0]).

start() ->
  register(ping, spawn(?MODULE, ping_init, [])),
  register(pong, spawn(?MODULE, pong_loop, [])).

stop() ->
  ping ! stop,
  pong ! stop.

play(N) ->
  ping ! {pong,N}.

ping_init() ->
  ping_loop(0).

ping_loop(Sum) ->
  receive
    stop -> ok;
    {_,0} -> ping_loop(Sum);
    {pong, N} ->
      timer:sleep(1000),
      io:format("Ping received ~p. Total sum: ~p ~n",[N, Sum+N]),
      pong ! {ping,(N-1)},
      ping_loop(Sum + N + N-1)
  after
    20000 -> ok
  end.

pong_loop() ->
  receive
    stop -> ok;
    {_, 0} -> pong_loop();
    {ping,N} ->
      timer:sleep(1000),
      io:format("Pong received ~p ~n",[N]),
      ping ! {pong,(N-1)},
      pong_loop()
  after
    20000 -> ok
  end.