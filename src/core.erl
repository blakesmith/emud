-module(core).
-behaviour(gen_server).
-export([listen/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

-include("client.hrl").

-define(GEN_TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true}]).

listen(Port) ->
	gen_server:start_link({local, emud}, ?MODULE, [Port], []).

stop() ->
	gen_server:call(?MODULE, stop).

init([Port]) ->
	io:fwrite("Emud started. Listening on port ~w...~n", [Port]),
	register(emud_client_manager, spawn(fun() -> manage_clients([]) end)),
	{ok, LSocket} = gen_tcp:listen(Port, ?GEN_TCP_OPTIONS),
	register(emud_listener, spawn(fun() -> do_accept(LSocket) end)),	
	{ok, listening}.

do_accept(LSocket) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	spawn(fun() -> handle_client(Socket) end),
	emud_client_manager ! {connect, Socket},
	do_accept(LSocket).

handle_client(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			gen_tcp:send(Socket, input_parse(Packet)),
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
			ok
	end,
	manage_clients(Clients).

input_parse(Packet) ->
	Packet.	

handle_call(stop, _From, Tab) ->
	{stop, normal, stopped, Tab}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
