-module(node_responder).
-export([start/2, stop/0]).

start(Port, ReplyPort) ->
	{ok, Socket} = gen_udp:open(Port),
	Pid = spawn(fun() -> listen(Socket, ReplyPort) end),
	register(node_listen, Pid),
	gen_udp:controlling_process(Socket, Pid).

stop() ->
	node_listen ! {self(), stop},
	receive
		Msg -> Msg
	end.

listen(Socket, ReplyPort) ->
	receive
		{udp, _, IP, Port, "ping"} ->
			io:format("Got a ping from ~w:~w~n", [IP, Port]),
			gen_udp:send(Socket,IP, ReplyPort, erlang:atom_to_list(node())),
			io:format("Sent ~w~n", [node()]),
			listen(Socket, ReplyPort);
		{From, stop} ->
			From ! gen_udp:close(Socket);
		Msg ->
			io:format("~w", [Msg]),
			listen(Socket, ReplyPort)
	end.
