-module(bs_integration).
-include_lib("eunit/include/eunit.hrl").
-include_lib("bs_const.hrl").

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

list_test() ->
    ?assertEqual([1,2,3], eval("(list 1 2 3)")).

vararg_lambda_test() ->
    ?assertEqual([1,2,3], eval("((lambda L L) 1 2 3)")).

quasiquote_test_() ->
    [
     ?_assertEqual([1,2,3], eval("(quasiquote (1 2 3))")),
     ?_assertEqual([1,2,3], eval("`(1 2 3)")),
     ?_assertEqual([1,2,3], eval("(set! x 2) (quasiquote (1 (unquote x) 3))")),
     ?_assertEqual([1,2,3], eval("(set! x 2) `(1 ,x 3)")),
     ?_assertEqual([1,23,3], eval("(set! x 11) (set! y 12) `(1 ,(+ x y) 3)")),
     ?_assertEqual([1, 11, [2, [5, 12]], 3], eval("(set! x 11) (set! y 12) `(1 ,x ,(list 2 `(5 ,y)) 3)")),
     ?_assertEqual([1,2,3,4], eval("(set! x '(2 3)) (quasiquote (1 (unquote-splicing x) 4))")),
     ?_assertEqual([1,2,3,4], eval("(set! x '(2 3)) `(1 ,@x 4)"))
    ].

define_syntax_test_() ->
    [
     ?_assertEqual(5, eval("(define-syntax five (lambda (x) 5)) five")),
     ?_assertEqual(5, eval("(define-syntax five (lambda (x) 5)) (five)")),
     ?_assertEqual(8, eval("(define-syntax c+ (lambda (x) (+ (list-ref x 1) (list-ref x 2)))) (c+ 3 5)"))
    ].

load_test() ->
    ?assertEqual(12, eval("(load \"../fixtures/x5y7.bs\")  (+ x y)")).

let_test() ->
    ?assertEqual(12, eval("(let ((x 5) (y 7)) (+ x y))")).

begin_test() ->
    ?assertEqual(5, eval("(begin 1 2 (+ 1 4))")).

implicit_begin_test_() ->
    [
     ?_assertEqual(2, eval("((lambda () 1 2))")),
     ?_assertEqual(2, eval("(let () 1 2)"))
    ].

comment_test_() ->
    [
     ?_assertEqual(5, eval("((lambda (x) ; foo\nx) ;; bar\n 5) ; end")),
     ?_assertEqual(4, eval("(+ 1 #;2 3)"))
    ].

square_brackets_test() ->
    ?assertEqual([1, 2], eval("'[1 2]")).
