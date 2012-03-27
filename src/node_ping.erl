-module(node_ping).
-export([start/3, stop/0, ping/0, adjust_interval/1]).

start(BroadcastIp, Port, Interval) ->
	{ok, Socket} = gen_udp:open(0, [{broadcast, true}]),
    register(node_ping, spawn(fun() -> ping({BroadcastIp, Socket, Port}, Interval) end)),
    ok.

stop() ->
    node_ping ! {self(), stop},
    receive
        Msg -> Msg
    after
        100 -> {error, 'time-out'}
    end.

ping() ->
	node_ping ! ping.

adjust_interval(Interval) ->
    node_ping ! {self(), adji, Interval},
    receive
        Msg -> Msg
    after
        100 -> {error, 'time-out'}
    end.


ping(Com, Interval = []) ->
    receive
        ping ->
            send_ping(Com),
            ping(Com, Interval);
        {From, stop} ->
            From ! stopped;
        {From, adji, NewInterval} ->
            From ! ok,
            ping(Com, NewInterval)
    end;

ping(Com, Interval) -> 
    receive
        ping ->
            send_ping(Com),
            ping(Com, Interval);
        {From, stop} ->
            From ! stopped;
        {From, adji, NewInterval} ->
            From ! ok,
            ping(Com, NewInterval)
    after
        Interval ->
            send_ping(Com),
        	ping(Com, Interval)	
    end.



send_ping({BroadcastIp, Socket, Port}) ->
	gen_udp:send(Socket, BroadcastIp, Port,"ping").
