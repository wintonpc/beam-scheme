-module(enum).
-export([make/1, yield/1, next/1, done/0, to_list/1, from_list/1, map/2, filter/2]).
-include_lib("eunit/include/eunit.hrl").

make(G) -> {enum, spawn(fun() -> G(), yield('ENUM_DONE') end)}.

yield(V) ->
    receive
        {next, Caller} ->
            Caller ! {next_value, self(), V},
            V
    end.

next({enum, Pid}) ->
    Alive = is_process_alive(Pid),
    if
        Alive ->
            Pid ! {next, self()},
            receive
                {next_value, Pid, V} -> V
            end;
        true -> 'ENUM_DONE'
    end.

done() -> 'ENUM_DONE'.

to_list(E) -> to_list(E, []).
to_list(E, Acc) ->
    case next(E) of
        'ENUM_DONE' -> lists:reverse(Acc);
        V -> to_list(E, [V|Acc])
    end.

from_list(List) ->
    make(fun() -> gen_from_list(List) end).

gen_from_list(List) ->
    case List of
        [] -> ok;
        [H|T] -> yield(H),
                 gen_from_list(T)
    end.

map(E, F) ->
    make(fun() -> walk(E, fun(V) -> yield(F(V)) end) end).

filter(E, Pred) ->
    make(fun() ->
                      walk(E, fun(V) -> case Pred(V) of
                                            true -> yield(V);
                                            false -> ok
                                        end
                              end)
              end).

walk(E, F) ->
    case next(E) of
        'ENUM_DONE' -> 'ENUM_DONE';
        V -> F(V),
             walk(E, F)
    end.
    

%%% TESTS %%%

%% naturals

naturals() -> make(fun gen_naturals/0).

gen_naturals() -> gen_naturals(1).
gen_naturals(N) ->
    yield(N),
    gen_naturals(N+1).


%% fib

fib() -> make(fun gen_fib/0).

gen_fib() ->
    yield(0),
    yield(1),
    gen_fib(0, 1).
gen_fib(A, B) ->
    C = A + B,
    yield(C),
    gen_fib(B, C).


%% squares

squares(E) -> make(fun() -> gen_squares(E) end).

square(X) -> X * X.

gen_squares(E) ->
    yield(square(next(E))),
    gen_squares(E).


%% four

four() -> make(fun() -> yield(1), yield(2), yield(3), yield(4) end).


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

to_list_test_() ->
    [
     ?_assertEqual([1,2,3,4], to_list(four()))
    ].

from_list_test_() ->
    [
     ?_assertEqual([1,2,3,4], to_list(from_list([1,2,3,4])))
    ].

empty_enum_test_() ->
    MakeEmpty = fun() -> make(fun() -> ok end) end,
    [
     ?_assertEqual([], to_list(MakeEmpty())),
     ?_assertEqual(done(), next(MakeEmpty()))
    ].

map_test_() ->
    [
     ?_assertEqual([1,4,9,16], to_list(map(four(), fun square/1)))
    ].

filter_test_() ->
    Even = fun(X) -> X rem 2 == 0 end,
    [
     ?_assertEqual([2,4], to_list(filter(four(), Even)))
    ].
