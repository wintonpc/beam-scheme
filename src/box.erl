-module(box).
-include_lib("eunit/include/eunit.hrl").
-export([make/1, set/2, get/1, run/1]).

make(Value) ->
    spawn(box, run, [Value]).

run(Value) ->
    receive
        {set, NewValue} ->
            run(NewValue);
        {get, Sender} ->
            Sender ! {self(), Value},
            run(Value)
    end.

set(Box, Value) ->
    Box ! {set, Value},
    ok.

get(Box) ->
    Box ! {get, self()},
    receive
        {Box, Value} ->
            Value
    end.

box_test() ->
    Box = box:make(1),
    ?assertEqual(1, box:get(Box)),
    ?assertEqual(ok, box:set(Box, 2)),
    ?assertEqual(2, box:get(Box)).
