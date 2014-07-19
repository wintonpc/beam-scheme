-module(bs_repl).
-export([run/0]).
-include_lib("eunit/include/eunit.hrl").

run() ->
    S = buffer_stream:make(),
    spawn_link(fun() -> watch_keyboard(S) end),    
    run(S).

run(S) ->
    io:format("~p~n", [S]),
    print(bs_read:read(S)),
    run(S).
    
watch_keyboard(S) ->
    Line = io:get_line("> "),
    io:format("Line: ~p~n", [Line]),
    buffer_stream:append(S, Line),
    watch_keyboard(S).

print(Exp) ->
    io:format("~p~n", [Exp]).
