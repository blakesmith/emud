-module(emud).
-behaviour(application).
-export([start/0, start/1, start/2, stop/1, stop/0]).

start(_Type, Args) ->
	gen_server:start_link({local, emud}, core, [Args], []).

start(Type) ->
	start(Type, {port, 3333}).

start() ->
	start(normal, {port, 3333}).

stop(_State) ->
	gen_server:call(emud, stop).

stop() ->
	stop(normal).

