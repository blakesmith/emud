-module(core).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

-include("client.hrl").

-define(GEN_TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true}]).

init([{port, Port}]) ->
	io:fwrite("Emud started. Listening on port ~w...~n", [Port]),
	register(emud_client_manager, spawn(fun() -> manage_clients([]) end)),
	{ok, LSocket} = gen_tcp:listen(Port, ?GEN_TCP_OPTIONS),
	register(emud_listener, spawn(fun() -> do_accept(LSocket) end)),	
	{ok, listening}.

do_accept(LSocket) ->
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			spawn(fun() -> handle_client(Socket) end),
			emud_client_manager ! {connect, Socket},
			do_accept(LSocket);
		{error, timeout} ->
			do_accept(LSocket);
		{error, closed} ->
			ok;
		{error, Reason} ->
			io:fwrite("Listener produced error ~w~n", [Reason]),
			do_accept(LSocket)
	end.

handle_client(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			gen_tcp:send(Socket, input_parser:parse(Packet)),
			handle_client(Socket);
		{error, _Reason} ->
			emud_client_manager ! {disconnect, Socket}
	end.

manage_clients(Clients) ->
	receive
		{connect, Socket} ->
			gen_tcp:send(Socket, "Connected.\n"),
			io:fwrite("~w connected~n", [Socket]);
		{disconnect, Socket} ->
			io:fwrite("~w disconnected~n", [Socket]),
			ok;
		{terminate, Reason} ->
			io:fwrite("Client manager terminated with reason ~w~n", [Reason])
	end,
	manage_clients(Clients).

handle_call(stop, From, Tab) ->
	io:fwrite("Shutting down...~n"),
	emud_listener ! {terminate, From, stopped},
	emud_client_manager ! {terminate, stopped},
	unregister(emud_listener),
	unregister(emud_client_manager),
	io:fwrite("Received listener shutdown message~n"),
	{stop, normal, stopped, Tab}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
