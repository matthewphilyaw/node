-module(node_registry).
-export([start/1, stop/0, flush_node_table/0, get_node_table/0, mark_node_false/1]).

start(Port) ->
	{ok, Socket} = gen_udp:open(Port),
	Pid = spawn(fun() -> listen(Socket, []) end),
	register(node_listen, Pid),
	gen_udp:controlling_process(Socket, Pid).

flush_node_table() ->
	node_listen ! {self(), flush},
	receive
		Reply -> Reply
	after
		1000 -> {error, noreply}
	end.

mark_node_false(Node) ->
	node_listen ! {self(), markFalse, Node},
	receive
		Reply -> Reply	
	after
		1000 -> {error, noreply}
	end.

get_node_table() ->
	node_listen ! {self(), getNodeTable},
	receive
		Reply -> Reply	
	after
		1000 -> {error, noreply}
	end.

stop() ->
	node_listen ! {self(), stop},
	receive
		Msg -> Msg
	end.

listen(Socket, []) ->
	TableId = ets:new(nodes, [ordered_set]),
	listen(Socket, TableId);

listen(Socket, TableId) ->
	receive
		{udp, _, IP, Port, Msg} ->
			ets:insert(TableId, {Msg,true}),
			%%io:format("Got a registration from ~w:~w~nMessage: ~w~n", [IP, Port, erlang:list_to_atom(Msg)]),
			listen(Socket,TableId);
		{From, stop} ->
			From ! gen_udp:close(Socket);
		{From, flush} ->
			From ! ets:delete_all_objects(TableId);
		{From, markFalse, Key} ->
			From ! ets:update_element(TableId, Key, {2, false});
		{From, getNodeTable} ->
			From ! TableId;
		Msg ->
			io:format("~w", [Msg]),
			listen(Socket, TableId)
	end.
