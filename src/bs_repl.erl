-module(bs_repl).
-export([run/0]).
-include_lib("eunit/include/eunit.hrl").

run() ->
    KeyboardBuffer = buffer_stream:make(),
    Watcher = spawn_link(fun() -> watch_keyboard(KeyboardBuffer) end),
    Env = bs_scheme:env(),
    bs_env:set(Env, exit, fun() -> exit(repl_exit) end),
    catch run(bs_read:read(KeyboardBuffer), Env).

run(ExprStream, Env) ->
    io:format("Scheme> "),
    bs_print:print(bs_compile:eval(stream:next(ExprStream), Env)),
    run(ExprStream, Env).
    
watch_keyboard(Buf) ->
    buffer_stream:append(Buf, io:get_line("")),
    watch_keyboard(Buf).
