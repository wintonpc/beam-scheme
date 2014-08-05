-module(bs_vm).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-export([vm/2]).

vm(X, Env) -> vm([], X, Env, [], []).

vm(A, X, E, R, S) ->
    %io:format("vm(~p, ~p, ~p, ~p, ~p)~n", [A, X, E, R, S]),
    case X of
        {halt} -> A;
        {constant, Obj, _X} ->
            vm(Obj, _X, E, R, S);
        {refer, Var, _X} ->
            vm(bs_env:lookup(Var, E), _X, E, R, S);
        {close, Vars, Body, _X} ->
            vm(make_closure(Body, E, Vars), _X, E, R, S);
        {frame, _X, Ret} -> vm(A, _X, E, [], make_frame(Ret, E, R, S));
        {argument, _X} -> vm(A, _X, E, [A|R], S);
        {apply} ->
            {Body, Env, Vars} = A,
            vm(A, Body, bs_env:extend(Env, Vars, R), [], S);
        {return} ->
            {_X, _E, _R, _S} = S,
            vm(A, _X, _E, _R, _S)
    end.
        
make_closure(Body, E, Vars) -> {Body, E, Vars}.

make_frame(X, E, R, S) -> {X, E, R, S}.
    
application_test_() ->
    Env = bs_env:empty(),
    [
     ?_assertEqual(1, vm(bs_compile:compile([[lambda, [a, b], a], 1, 2], Env), Env)),
     ?_assertEqual(2, vm(bs_compile:compile([[lambda, [a, b], b], 1, 2], Env), Env))
    ].

full_stack_test_() ->
    [
     ?_assertEqual(1, bs_compile:eval(bs_read:read1("((lambda (a b) a) 1 2)"))),
     ?_assertEqual(2, bs_compile:eval(bs_read:read1("((lambda (a b) b) 1 2)")))
    ].
    

