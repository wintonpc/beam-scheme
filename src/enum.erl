-module(enum).
-export([make_enum/1, yield/1, next/1]).
-include_lib("eunit/include/eunit.hrl").

make_enum(G) -> {enum, spawn(G)}.

yield(V) ->
    receive
        {next, Caller} ->
            Caller ! {next_value, self(), V},
            V
    end.

next({enum, Pid}) ->
    Pid ! {next, self()},
    receive
        {next_value, Pid, V} -> V
    end.



%%% TESTS %%%

%% naturals

naturals() -> make_enum(fun gen_naturals/0).

gen_naturals() -> gen_naturals(1).
gen_naturals(N) ->
    yield(N),
    gen_naturals(N+1).


%% fib

fib() -> make_enum(fun gen_fib/0).

gen_fib() ->
    yield(0),
    yield(1),
    gen_fib(0, 1).
gen_fib(A, B) ->
    C = A + B,
    yield(C),
    gen_fib(B, C).


%% squares

squares(E) -> make_enum(fun() -> gen_squares(E) end).

square(X) -> X * X.

gen_squares(E) ->
    yield(square(next(E))),
    gen_squares(E).

%% tests

enum_test_() ->
    N = naturals(),
    F = fib(),
    [
     ?_assertEqual(1, next(N)),
     ?_assertEqual(2, next(N)),
     ?_assertEqual(3, next(N)),
     ?_assertEqual(4, next(N)),

     ?_assertEqual(0, next(F)),
     ?_assertEqual(1, next(F)),
     ?_assertEqual(1, next(F)),
     ?_assertEqual(2, next(F)),
     ?_assertEqual(3, next(F)),
     ?_assertEqual(5, next(F)),
     ?_assertEqual(8, next(F)),
     ?_assertEqual(13, next(F))
    ].

compose_enum_test_() ->
    Sn = squares(naturals()),
    Sf = squares(fib()),
    [
     ?_assertEqual(1, next(Sn)),
     ?_assertEqual(4, next(Sn)),
     ?_assertEqual(9, next(Sn)),
     ?_assertEqual(16, next(Sn)),

     ?_assertEqual(0, next(Sf)),
     ?_assertEqual(1, next(Sf)),
     ?_assertEqual(1, next(Sf)),
     ?_assertEqual(4, next(Sf)),
     ?_assertEqual(9, next(Sf)),
     ?_assertEqual(25, next(Sf)),
     ?_assertEqual(64, next(Sf)),
     ?_assertEqual(169, next(Sf))
    ].
