-module(buffer_stream).
-export([make/0, append/2, length/1]).
-include_lib("eunit/include/eunit.hrl").

make() -> {stream, spawn_link(fun run/0)}.

run() -> run(nil, []).

append({stream, Pid}, Data) ->
    Pid ! {append_data, Data}.

length({stream, Pid}) ->
    Pid ! {get_length, self()},
    receive
        {length, Pid, Length} -> Length
    end.

run(Caller, [B|Rest]) when is_pid(Caller) ->
    Caller ! {next_value, self(), B},
    run(nil, Rest);


run(OldCaller, Buffer) ->
    receive
        {append_data, Data} ->
            run(OldCaller, Buffer ++ Data);
        {get_length, Caller} ->
            Caller ! {length, self(), erlang:length(Buffer)},
            run(OldCaller, Buffer);
        {next, NewCaller} ->
            if is_pid(OldCaller) ->
                    error({multiple_callers, OldCaller});
               true ->
                    run(NewCaller, Buffer)
            end
    end.



%%% TESTS %%%

read_test_() ->
    S = make(),
    append(S, [1, 2]),
    append(S, [3, 4]),
    [
     ?_assertEqual(1, stream:next(S)),
     ?_assertEqual(2, stream:next(S)),
     ?_assertEqual(3, stream:next(S)),
     ?_assertEqual(4, stream:next(S))
    ].

blocking_read_test_() ->
    S = make(),
    spawn(fun() -> timer:sleep(20), append(S, [1, 2]) end),
    [
     ?_assertEqual(1, stream:next(S)),
     ?_assertEqual(2, stream:next(S))
    ].

multiple_readers_error_test() ->
    S = make(),
    OtherPid = spawn(fun() -> stream:next(S) end),
    timer:sleep(20),
    ?assertError({multiple_callers, OtherPid}, stream:next(S)).
                  
length_test() ->
    S = make(),
    append(S, [4,4,4]),
    ?assertEqual(3, buffer_stream:length(S)).
