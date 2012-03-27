-module(node_registry).
-export([start/1, stop/0, flush_node_table/0, get_node_table/0, mark_node_false/1]).

start(Port) ->
	{ok, Socket} = gen_udp:open(Port),
	register(node_ets, spawn(fun() -> ets_loop([]) end)),
	register(node_listen, spawn(fun() -> listen(Socket) end)),
	gen_udp:controlling_process(Socket, erlang:whereis(node_listen)).

flush_node_table() ->
	node_ets ! {self(), flush},
	receive
		Reply -> Reply
	after
		1000 -> {error, noreply}
	end.

mark_node_false(Node) ->
	node_ets ! {self(), markFalse, Node},
	receive
		Reply -> Reply	
	after
		1000 -> {error, noreply}
	end.

get_node_table() ->
	node_ets ! {self(), getNodeTable},
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

ets_loop([]) ->
	TableId = ets:new(nodes, [ordered_set]),
	ets_loop(TableId);

ets_loop(TableId) ->
	receive
		{From, flush} ->
			From ! ets:delete_all_objects(TableId),
			ets_loop(TableId);
		{From, markFalse, Key} ->
			From ! ets:update_element(TableId, Key, {2, false}),
			ets_loop(TableId);
		{From, getNodeTable} ->
			From ! TableId,
			ets_loop(TableId);
		{From, insert, Node} ->
			ets:insert(TableId, Node),
			ets_loop(TableId);
		Msg -> ok, ets_loop(TableId)
	end.	

listen(Socket) ->
	receive
		{udp, _, IP, Port, Msg} ->
			%%io:format("Got a registration from ~w:~w~nMessage: ~w~n", [IP, Port, erlang:list_to_atom(Msg)]),
			etsproc ! {self(), insert, {Msg,true}},
			listen(Socket);
		{From, stop} ->
			From ! gen_udp:close(Socket);
		Msg ->
			io:format("~w", [Msg]),
			listen(Socket)
	end.
