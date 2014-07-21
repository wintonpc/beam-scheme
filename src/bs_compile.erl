-module(bs_compile).
-include_lib("eunit/include/eunit.hrl").
-export([compile/1, eval/1, eval/2]).

eval(X) -> eval(X, bs_env:empty()).

eval(X, Env) -> bs_vm:vm(compile(X), Env).

compile(X) -> compile(X, {halt}).

compile(X, Next) when is_atom(X) ->
    {refer, X, Next};

compile([quote, Obj], Next) ->
    {constant, Obj, Next};

compile([lambda, Vars, Body], Next) ->
    {close, Vars, compile(Body, {return}), Next};

compile([Op|Args], Next) ->
    Compiled = compile_args(Args, compile(Op, {apply})),
    case is_tail(Next) of
        true -> Compiled;
        false -> {frame, Compiled, Next}
    end;

compile(X, Next) ->
    {constant, X, Next}.


compile_args([], Compiled) -> Compiled;
compile_args([Arg|Rest], Compiled) ->
    compile_args(Rest, compile(Arg, {argument, Compiled})).

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
