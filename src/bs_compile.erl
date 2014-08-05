-module(bs_compile).
-include_lib("eunit/include/eunit.hrl").
-export([compile/2, eval/1, eval/2]).

eval(X) -> eval(X, bs_env:empty()).

eval(X, Env) -> bs_vm:vm(compile(X, Env), Env).

compile(X) -> compile(X, bs_env:empty()).

compile(X, Env) -> compile(X, {halt}, {bs_env:all_vars(Env), nil}).

compile(X, Next, VarRibs) when is_atom(X) ->
    {refer, X, Next};

compile([quote, Obj], Next, VarRibs) ->
    {constant, Obj, Next};

compile([lambda, Vars, Body], Next, VarRibs) ->
    {close, Vars, compile(Body, {return}, VarRibs), Next};

compile([Op|Args], Next, VarRibs) ->
    Compiled = compile_args(Args, compile(Op, {apply}, VarRibs), VarRibs),
    case is_tail(Next) of
        true -> Compiled;
        false -> {frame, Compiled, Next}
    end;

compile(X, Next, VarRibs) ->
    {constant, X, Next}.


compile_args([], Compiled, VarRibs) -> Compiled;
compile_args([Arg|Rest], Compiled, VarRibs) ->
    compile_args(Rest, compile(Arg, {argument, Compiled}, VarRibs), VarRibs).

is_tail({return}) -> true;
is_tail(_) -> false.

    

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
