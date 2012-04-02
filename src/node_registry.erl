-module(node_registry).
-export([start/2, stop/0]).
-export([register_pid/1,unregister_pid/1]).

-define(TIMEOUT, 1000).

%% API

start(Port, Pids) ->
    {ok, Socket} = gen_udp:open(Port),
    register(node_registry, spawn(fun() -> 
                                    listen({Socket, Pids})
                                  end)),
    gen_udp:controlling_process(Socket, erlang:whereis(node_registry)).

register_pid(Pid) ->
    send({reg, Pid}).

unregister_pid(Pid) ->
    send({del, Pid}).

stop() ->
    send({stop}).

%% loops

listen(State = {Socket, Pids}) ->
    receive
        {udp, _, _, _, [35,35,110,111,100,101,35,35 | Node]} ->
            notify(Pids, Node),
            listen(State);  
        {udp, _, IP, Port, Msg} ->
            io:format("Received junk data from ~w:~w~nData: ~w~n", [IP, Port, Msg]),
            listen(State);
        {From, {stop}} ->
            From ! ok,
            gen_udp:close(Socket);
        {From, {reg, Pid}} -> 
            case duplicate_pid(Pid, Pids) of
                true -> 
                    From ! {error, already_registered},
                    listen(State);
                false -> 
                    From ! ok,
                    listen({Socket, [Pid | Pids]})
            end;
        {From, {del, Pid}} ->
            From ! ok,
            listen({Socket, lists:delete(Pid, Pids)});
        Msg ->
            io:format("~w~n", [Msg]),
            {error, <<"Uknown Message">>}
    end.

%% private

send(Command) ->
    node_registry ! {self(), Command},
    receive
        Msg -> Msg
    after
        ?TIMEOUT ->
            {error, timeout}
    end.

notify([], _Node) -> ok;
notify([Pid | Rest], Node) ->
    Pid ! {newnode, list_to_atom(Node)},
    notify(Rest, Node).

duplicate_pid(Pid, Pids) ->
    Exists = lists:foldl(fun(P, Match) -> 
                            case P == Pid of
                                true -> Match + 1;
                                false -> Match
                            end
                         end, 0, Pids),
    Exists > 0.
        
