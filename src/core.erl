-module(core).
-export([listen/1]).

-include("client.hrl").

-define(GEN_TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true}]).

listen(Port) ->
	io:fwrite("Emud started. Listening on port ~w...~n", [Port]),
	register(client_manager, spawn(fun() -> manage_clients([]) end)),
	{ok, LSocket} = gen_tcp:listen(Port, ?GEN_TCP_OPTIONS),
	do_accept(LSocket).	

do_accept(LSocket) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	spawn(fun() -> handle_client(Socket) end),
	client_manager ! {connect, Socket},
	do_accept(LSocket).

handle_client(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			gen_tcp:send(Socket, input_parse(Packet)),
			handle_client(Socket);
		{error, _Reason} ->
			client_manager ! {disconnect, Socket}
	end.

manage_clients(Clients) ->
	receive
		{connect, Socket} ->
			gen_tcp:send(Socket, "Connected.\n"),
			io:fwrite("~w connected~n", [Socket]);
		{disconnect, Socket} ->
			io:fwrite("~w disconnected~n", [Socket]),
			ok
	end,
	manage_clients(Clients).

input_parse(Packet) ->
	Packet.	

