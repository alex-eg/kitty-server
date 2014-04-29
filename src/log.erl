-module(log).
-export([info/1,info/2]).

info(Args) ->
    io:format("~ts~n", [Args]).

info(Format, Args) ->
    Record = io_lib:format(Format,Args),
    io:format("~s~n", [Record]).
