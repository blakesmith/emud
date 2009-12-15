-module(main).

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

handle_client(Client) ->
	case gen_tcp:recv(Client#client.socket, 0) of
		{ok, Packet} ->
			gen_tcp:send(Client#client.socket, input_parser:parse(Packet)),
			handle_client(Client);
		{error, _Reason} ->
			emud_client_manager ! {disconnect, Client}
	end.

manage_clients(Clients) ->
	receive
		{connect, Socket} ->
			Client = #client{login=unspecified, authed=false, socket=Socket},
			WithNewClient = [Client|Clients],
			gen_tcp:send(Client#client.socket, "Connected.\n"),
			io:fwrite("~w connected. Client count: ~w~n", [Client#client.socket, length(WithNewClient)]),
			spawn(fun() -> handle_client(Client) end),
			manage_clients(WithNewClient);
		{disconnect, Client} ->
			WithoutClient = delete_client_by_socket(Clients, Client#client.socket),
			io:fwrite("~w disconnected. Client count: ~w~n", [Client#client.socket, length(WithoutClient)]),
			manage_clients(WithoutClient);
		{terminate, Reason} ->
			io:fwrite("Client manager terminated with reason ~w~n", [Reason]),
			ok
	end.

delete_client_by_socket(Clients, Socket) ->
	lists:filter(fun(X) -> X#client.socket /= Socket end, Clients).

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
