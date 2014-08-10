-module(bs_compile).
-include_lib("eunit/include/eunit.hrl").
-export([compile/2, eval/1, eval/2]).

eval(X) -> eval(X, bs_env:empty()).

eval(X, Env) ->
    Compiled = compile(X, Env),
    io:format(user, "~p~n", [Compiled]),
    bs_vm:vm(Compiled, Env).

compile(X) -> compile(X, bs_env:empty()).

compile(X, Env) -> compile(X, {halt}, {Env, nil}).

compile(X, Next, _) when is_atom(X) ->
    {refer, X, Next};

compile([quote, Obj], Next, _) ->
    {constant, Obj, Next};

compile([lambda, Vars, Body], Next, {Env, VarRibs}) ->
    {close, Vars, compile(Body, {return}, {Env, add_rib(Vars, VarRibs)}), Next};

compile([Op|Args], Next, {Env, VarRibs}) when is_atom(Op) ->
    case is_free_identifier(Op, VarRibs) of
        false -> compile_application([Op|Args], Next, {Env, VarRibs});
        true ->
            case bs_env:try_lookup(Op, Env) of
                not_found -> compile_application([Op|Args], Next, {Env, VarRibs}); % throw error?
                {found, Fun} when is_function(Fun) ->
                    compile_args(Args, {apply_primop_tag(Fun), Fun, Next}, {Env, VarRibs})
            end
    end;

compile(X, Next, EnvInfo) when is_list(X) ->
    compile_application(X, Next, EnvInfo);

compile(X, Next, _) ->
    {constant, X, Next}.

compile_application([Op|Args], Next, EnvInfo) ->
    Compiled = compile_args(Args, compile(Op, {apply}, EnvInfo), EnvInfo),
    case is_tail(Next) of
        true -> Compiled;
        false -> {frame, Compiled, Next}
    end.

compile_args([], Compiled, _) -> Compiled;
compile_args([Arg|Rest], Compiled, EnvInfo) ->
    compile_args(Rest, compile(Arg, {argument, Compiled}, EnvInfo), EnvInfo).

is_tail({return}) -> true;
is_tail(_) -> false.

add_rib(Vars, VarRibs) ->
    {Vars, VarRibs}.

is_free_identifier(Id, VarRibs) ->
    not is_bound_identifier(Id, VarRibs).

is_bound_identifier(_, nil) ->
    false;
is_bound_identifier(Id, {Vars, Parent}) ->
    lists:member(Id, Vars) orelse is_bound_identifier(Id, Parent).
    
apply_primop_tag(Fun) when is_function(Fun) ->
    case erlang:fun_info(Fun, arity) of
        {arity, 0} -> apply_primop_0;
        {arity, 1} -> apply_primop_1;
        {arity, 2} -> apply_primop_2;
        {arity, 3} -> apply_primop_3
    end.

compile_symbol_test() ->
    ?assertEqual({refer, foo, {halt}}, compile(foo)).

compile_integer_test() ->
    ?assertEqual({constant, 1, {halt}}, compile(1)).

compile_quote_test() ->
    ?assertEqual({constant, [foo, bar], {halt}}, compile([quote, [foo, bar]])).

compile_lambda_test() ->
    ?assertEqual({close, [x], {refer, x, {return}}, {halt}}, compile([lambda, [x], x])).

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
