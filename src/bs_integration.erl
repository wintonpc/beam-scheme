-module(bs_integration).
-include_lib("eunit/include/eunit.hrl").

apply_lambda_test() ->
    Expr = bs_read:read1("((lambda (a b) b) 1 2)"),
    Evaluated = bs_compile:eval(Expr),
    ?assertEqual(2, Evaluated).

set_top_level_test() ->
    Env = bs_env:empty(),
    Exprs = bs_read:read_all("(set! x 5)   x"),
    Result = lists:foldl(fun(Expr, _) -> bs_compile:eval(Expr, Env) end, nil, Exprs),
    ?assertEqual(5, Result).

primop_test() ->
    Expr = bs_read:read1("(+ (- 7 4) 10)"),
    Evaluated = bs_compile:eval(Expr, bs_scheme:env()),
    ?assertEqual(13, Evaluated).


if_test() ->
    Expr = bs_read:read1("(if (quote a) 1 2)"),
    ?assertEqual(1, bs_compile:eval(Expr)).

