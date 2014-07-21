-module(bs_repl).
-export([run/0]).
-include_lib("eunit/include/eunit.hrl").

run() ->
    KeyboardBuffer = buffer_stream:make(),
    spawn_link(fun() -> watch_keyboard(KeyboardBuffer) end),    
    run(bs_read:read(KeyboardBuffer), bs_scheme:env()).

run(ExprStream, Env) ->
    bs_print:print(bs_compile:eval(stream:next(ExprStream), Env)),
    run(ExprStream, Env).
    
watch_keyboard(Buf) ->
    buffer_stream:append(Buf, io:get_line("Scheme> ")),
    watch_keyboard(Buf).
