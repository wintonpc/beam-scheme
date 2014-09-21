-module(bs_vm).
-include("bs_const.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-export([vm/2]).

vm(X, Env) ->
    %io:format(user, "Running: ~p~n", [X]),
    vm(?VOID, X, Env, [], []).

vm(A, X, E, R, S) ->
    %io:format("vm(~p, ~p, ~p, ~p, ~p)~n", [A, X, E, R, S]),
    case X of
        {halt} -> A;
        {constant, Obj, _X} ->
            vm(Obj, _X, E, R, S);
        {refer, Var, _X} ->
            vm(bs_env:lookup(Var, E), _X, E, R, S);
        {close, Vars, Body, _X} ->
            vm(make_closure(Vars, Body, E), _X, E, R, S);
        {frame, _X, Ret} -> vm(A, _X, E, [], make_frame(Ret, E, R, S));
        {argument, _X} -> vm(A, _X, E, [A|R], S);
        {arguments, _X} -> vm(A, _X, E, A, S);
        {assign, Var, _X} ->
            bs_env:set(E, Var, A),
            vm(?VOID, _X, E, R, S);
        {test, Then, Else} ->
            vm(A, case A of ?FALSE -> Else; _ -> Then end, E, R, S);
        {apply} ->
            case A of
                {closure, Body, Env, Vars} ->
                    {MVars, MVals} = lists:unzip(map_formals(Vars, R)),
                    vm(A, Body, bs_env:extend(Env, MVars, MVals), [], S);
                {primop, 0, Fun} ->
                    case R of
                        [] -> vm(Fun(), {return}, E, R, S);
                        Rs -> error({wrong_arg_count, 0, Rs})
                     end;
                {primop, 1, Fun} ->
                    case R of
                        [R1] -> vm(Fun(R1), {return}, E, R, S);
                        Rs -> error({wrong_arg_count, 1, Rs})
                    end;
                {primop, 2, Fun} ->
                    case R of
                        [R1, R2] -> vm(Fun(R1, R2), {return}, E, R, S);
                        Rs -> error({wrong_arg_count, 2, Rs})
                    end;
                {primop, 3, Fun} ->
                    case R of
                        [R1, R2, R3] -> vm(Fun(R1, R2, R3), {return}, E, R, S);
                        Rs -> error({wrong_arg_count, 3, Rs})
                    end;
                Unk -> error({tried_to_apply_non_procedure, Unk})
            end;
        {return} ->
            {_X, _E, _R, _S} = S,
            vm(A, _X, _E, _R, _S)
    end.

map_formals(F, Vals) when is_atom(F) -> [{F, Vals}];
map_formals([], []) -> [];
map_formals([F|Rf], [V|Rv]) -> [{F,V}|map_formals(Rf, Rv)].

make_closure(Vars, Body, E) -> {closure, Body, E, Vars}.

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

if_test() ->
    ?assertEqual(2, bs_compile:eval(bs_read:read1("((lambda (x) (if x 1 2)) #f)"))).
    
