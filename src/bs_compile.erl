-module(bs_compile).
-include_lib("eunit/include/eunit.hrl").
-export([compile/2, eval/1, eval/2]).

eval(X) -> eval(X, bs_env:empty()).

eval(X, Env) ->
    Compiled = compile(X, Env),
    %io:format(user, "~p~n", [Compiled]),
    bs_vm:vm(Compiled, Env).

compile(X) -> compile(X, bs_env:empty()).

compile(X, Env) -> compile(X, {halt}, {Env, nil}).

compile(X, Next, {Env, VarRibs}) when is_atom(X) ->
    %io:format(user, "compiling a reference~n", []),
    case {is_free_identifier(X, VarRibs), bs_env:try_lookup(X, Env)} of
        {true, {found, Fun}} when is_function(Fun) ->
            %io:format(user, "compiling primop~n", []),
            {constant, {primop, arity(Fun), Fun}, Next};
        _ ->
            %io:format(user, "reference: ~p~n", [Foo]),
            {refer, X, Next}
    end;

compile([quote, Obj], Next, _) ->
    {constant, Obj, Next};

compile([lambda, Vars, Body], Next, {Env, VarRibs}) ->
    {close, Vars, compile(Body, {return}, {Env, add_rib(Vars, VarRibs)}), Next};

compile(['if', Test, Then, Else], Next, EnvInfo) ->
    ThenC = compile(Then, Next, EnvInfo),
    ElseC = compile(Else, Next, EnvInfo),
    compile(Test, {test, ThenC, ElseC}, EnvInfo);

compile(['set!', Var, X], Next, EnvInfo) ->
    compile(X, {assign, Var, Next}, EnvInfo);

compile([Op|Args], Next, EnvInfo) ->
    Compiled = compile_args(Args, compile(Op, {apply}, EnvInfo), EnvInfo),
    case is_tail(Next) of
        true -> Compiled;
        false -> {frame, Compiled, Next}
    end;

compile(X, Next, _) ->
    {constant, X, Next}.


compile_args([], Compiled, _) -> Compiled;
compile_args([Arg|Rest], Compiled, EnvInfo) ->
    compile_args(Rest, compile(Arg, {argument, Compiled}, EnvInfo), EnvInfo).

is_tail({return}) -> true;
is_tail(_) -> false.

add_rib(L, VarRibs) when is_atom(L) ->
    {[L], VarRibs};
add_rib(Vars, VarRibs) ->
    {Vars, VarRibs}.

is_free_identifier(Id, VarRibs) ->
    not is_bound_identifier(Id, VarRibs).

is_bound_identifier(_, nil) ->
    false;
is_bound_identifier(Id, {Vars, Parent}) ->
    lists:member(Id, Vars) orelse is_bound_identifier(Id, Parent).
    
arity(Fun) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    Arity.

compile_symbol_test() ->
    ?assertEqual({refer, foo, {halt}}, compile(foo)).

compile_integer_test() ->
    ?assertEqual({constant, 1, {halt}}, compile(1)).

compile_quote_test() ->
    ?assertEqual({constant, [foo, bar], {halt}}, compile([quote, [foo, bar]])).

compile_lambda_test() ->
    ?assertEqual({close, [x], {refer, x, {return}}, {halt}}, compile([lambda, [x], x])).

compile_if_test() ->
    ?assertEqual({constant, 5, {test, {constant, 1, {halt}}, {constant, 2, {halt}}}}, compile(['if', 5, 1, 2])).

compile_set_test() ->
    ?assertEqual({constant, 5, {assign, x, {halt}}}, compile(['set!', x, 5])).

compile_application_test() ->
    ?assertEqual({frame,
                  {constant, 2,
                   {argument,
                    {constant, 1,
                     {argument,
                      {close, [a, b], {refer, a, {return}}, {apply}}}}}},
                  {halt}},
                 compile([[lambda, [a, b], a], 1, 2])).

is_free_identifier_test_() ->
    VarRibs = {[a, b], {[c, d], nil}},
    [
     ?_assert(false == is_free_identifier(a, VarRibs)),
     ?_assert(false == is_free_identifier(b, VarRibs)),
     ?_assert(false == is_free_identifier(c, VarRibs)),
     ?_assert(false == is_free_identifier(d, VarRibs)),
     ?_assert(true == is_free_identifier(e, VarRibs))
    ].
