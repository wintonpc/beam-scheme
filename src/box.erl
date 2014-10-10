-module(box).
-include_lib("eunit/include/eunit.hrl").
-export([make/1, set/2, get/1, run/1, is_box/1, unbox/1]).

make(Value) ->
    {box, spawn(box, run, [Value])}.

run(Value) ->
    receive
        {set, NewValue} ->
            run(NewValue);
        {get, Sender} ->
            Sender ! {self(), Value},
            run(Value)
    end.

set({box, Pid}, Value) ->
    Pid ! {set, Value},
    ok.

get({box, Pid}) ->
    Pid ! {get, self()},
    receive
        {Pid, Value} ->
            Value
    end.

is_box({box, _}) -> true;
is_box(_) -> false.

unbox({box, Pid}) -> box:get({box, Pid});
unbox(X) -> X.

box_test_() ->
    Box = box:make(1),
    [
     ?_assertEqual(1, box:get(Box)),
     ?_assertEqual(ok, box:set(Box, 2)),
     ?_assertEqual(2, box:get(Box)),
     ?_assertEqual(true, box:is_box(Box)),
     ?_assertEqual(false, box:is_box(1)),
     ?_assertEqual(2, box:unbox(Box)),
     ?_assertEqual(3, box:unbox(3))
    ].
