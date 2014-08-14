-module(bs_integration).
-include_lib("eunit/include/eunit.hrl").

apply_lambda_test() ->
    Expr = bs_read:read1("((lambda (a b) b) 1 2)"),
    Evaluated = bs_compile:eval(Expr),
    ?assertEqual(2, Evaluated).

set_top_level_test() ->
    ?assertEqual(5, eval("(set! x 5)   x")).

primop_test() ->
    Expr = bs_read:read1("(+ (- 7 4) 10)"),
    Evaluated = bs_compile:eval(Expr, bs_scheme:env()),
    ?assertEqual(13, Evaluated).


if_test() ->
    Expr = bs_read:read1("(if (quote a) 1 2)"),
    ?assertEqual(1, bs_compile:eval(Expr)).

eval(S) ->
    Env = bs_scheme:env(),
    Exprs = bs_read:read_all(S),
    Result = lists:foldl(fun(Expr, _) -> bs_compile:eval(Expr, Env) end, nil, Exprs),
    Result.

fact_test() ->
    ?assertEqual(120, eval("(set! fact (lambda (x) (if (= x 1) 1 (* x (fact (- x 1))))))   (fact 5)")).
