-module(node).
-export([start/0, stop/0]).

start() ->
    Pid = spawn(fun() -> loop() end),
    register(node, Pid),
    node_ping:start("192.168.1.255", 9998, 10000),
    node_responder:start(9998, 9999),
    node_registry:start(9999, [Pid]).

stop() ->
    node ! stop,
    ok. 

loop() ->
    ThisNode = node(),
    receive
        stop ->
            ok;
        {newnode, ThisNode} -> 
            loop();
        {newnode, Node} ->
            io:format("Found a new node: ~w", [Node]),
            loop();
        _ -> 
            loop()       
    end.
