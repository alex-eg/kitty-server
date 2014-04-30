-module(kitty).

-export([start/1]).

-include("records.hrl").

start([server]) ->
    {ok, [Config]} = file:consult("priv/cfg.erl"),
    Addr = proplists:get_value(bind_addr, Config),
    Port = proplists:get_value(listen_port, Config),
    start(server, {Addr, Port});
start([client]) ->
    {ok, [Config]} = file:consult("priv/cfg.erl"),
    Addr = proplists:get_value(connect_addr, Config),
    Port = proplists:get_value(connect_port, Config),
    start(client, {Addr, Port});
start([_Any]) ->
    io:fwrite("What do you think you're doing, Dave?~n"),
    normal.

start(client, {ServerAddr, ServerPort}) ->
    State = #state{addr = ServerAddr, port = ServerPort, type = client},
    {ok, _Pid} = gen_server:start_link({local, State#state.type},
                                       State#state.type, State, []),
    start_io(State);
start(server, {Addr, Port}) ->
    State = #state{addr = Addr, port = Port, type = server},
    {ok, _Pid} = gen_server:start_link({local, State#state.type},
                                       State#state.type, State, []),
    start_io(State).

start_io(State) ->
    Line = io:get_line("?-- "),
    send_line(Line, State).

send_line(eof, State) ->
    io:fwrite("~nGood bye!~n"),
    gen_server:call(State#state.type, shutdown),
    normal;
send_line(Line, State) ->
    gen_server:cast(State#state.type, {send, Line}),
    start_io(State).
