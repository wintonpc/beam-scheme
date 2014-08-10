-module(bs_integration).
-include_lib("eunit/include/eunit.hrl").

apply_lambda_test() ->
    Expr = bs_read:read1("((lambda (a b) b) 1 2)"),
    Evaluated = bs_compile:eval(Expr),
    ?assertEqual(2, Evaluated).

set_top_level_test() ->
    Env = bs_env:empty(),
    SetExpr = bs_read:read1("(set! x 5)"),
    ReadExpr = bs_Read:read1("x"),
    bs_compile:eval(SetExpr, Env),
    Evaluated = bs_compile:eval(ReadExpr, Env),
    ?assertEqual(5, Evaluated).

primop_test() ->
    Expr = bs_read:read1("(+ (- 7 4) 10)"),
    Evaluated = bs_compile:eval(Expr, bs_scheme:env()),
    ?assertEqual(13, Evaluated).

