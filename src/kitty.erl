-module(kitty).

-export([start/1]).

-include("records.hrl").

start([server]) ->
    start(server, {{127,0,0,1}, 3456});
start([client]) ->
    start(client, {{127,0,0,1}, 3456});
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
    Line = io:get_line("?- "),
    send_line(Line, State).

send_line(eof, _) ->
    io:fwrite("~nGood bye!~n"),
    normal;
send_line(Line, State) ->
    gen_server:cast(State#state.type, {send, Line}),
    start_io(State).
