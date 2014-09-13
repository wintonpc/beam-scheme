-module(bs_scheme).
-export([env/0]).
-include("bs_const.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

env() ->
    E = bs_env:empty(),
    Eval = fun(Code) -> bs_compile:eval(bs_read:read1(Code), E) end,
    GensymNumbers = stream:naturals(),
    bs_env:set(E, '+', fun erlang:'+'/2),
    bs_env:set(E, '-', fun erlang:'-'/2),
    bs_env:set(E, '*', fun erlang:'*'/2),
    bs_env:set(E, '=', fun erlang:'=='/2),
    bs_env:set(E, 'equal?', fun erlang:'=='/2),
    bs_env:set(E, 'cons', fun(A, B) -> [A|B] end),
    bs_env:set(E, 'car', fun erlang:hd/1),
    bs_env:set(E, 'cdr', fun erlang:tl/1),
    bs_env:set(E, 'null?', fun null_p/1),
    bs_env:set(E, 'list?', fun bs_scheme:list_p/1),
    bs_env:set(E, 'pair?', fun bs_scheme:pair_p/1),
    bs_env:set(E, 'append-lists', fun lists:append/1),
    bs_env:set(E, 'length', fun erlang:length/1),
    bs_env:set(E, '#%gensym', fun(L) -> gensym(L, GensymNumbers) end),
    bs_env:set(E, '#%let-transformer', fun transform_let/1),
    Eval("(define-syntax let #%let-transformer)"),
    bs_env:set(E, 'cwd', fun() -> {ok, Path} = file:get_cwd(), list_to_binary(Path) end),
    bs_env:set(E, 'list-ref', fun(List, N) -> lists:nth(N + 1, List) end),
    bs_env:set(E, 'load', fun(File) -> load(File, E) end),
    bs_env:set(E, 'expand', fun(Expr, Keywords) -> expand(Expr, Keywords, E) end),

    load("/home/pwinton/git/bs/src/scheme0.bs", E),

    E.
    

load(File, Env) ->
    %io:format(user, "in '~p', opening '~p'~n", [file:get_cwd(), File]),
    {ok, Content} = file:read_file(File),
    ExprStream = bs_read:read(stream:from_list(binary_to_list(Content))),
    eval_all(ExprStream, Env),
    ?VOID.

eval_all(ExprStream, Env) ->
    StreamDone = stream:done(),
    case stream:next(ExprStream) of
        StreamDone -> ok;
        Expr ->
            bs_print:print(bs_compile:eval(Expr, Env)),
            eval_all(ExprStream, Env)
    end.

% gensym() ->
%     {A, B, C} = erlang:now(),
%     %list_to_atom(lists:flatten(io_lib:format("gensym-~p-~p-~p-~p", [erlang:node(), A, B, C]))).
%     list_to_atom(lists:flatten(io_lib:format("gensym-~p-~p-~p", [A, B, C]))).

gensym([], Numbers) ->
    gensym([g], Numbers);
gensym([Name], Numbers) ->
    list_to_atom(lists:flatten(io_lib:format("^~p~p", [Name, stream:next(Numbers)]))).

null_p([]) -> true;
null_p(_) -> false.

list_p([]) -> true;
list_p([_|X]) -> list_p(X);
list_p(_) -> false.

pair_p([_|_]) -> true;
pair_p(_) -> false.

first([X|_]) -> X.
second([_, X|_]) -> X.

transform_let([_, Bindings, Body]) ->
    Names = lists:map(fun first/1, Bindings),
    Exprs = lists:map(fun second/1, Bindings),
    [[lambda, Names, Body]|Exprs].

expand(Expr, Keywords, Env) -> expand(Expr, [], Keywords, Env).

expand(Last, Last, _, _) -> Last;
expand(Stx, _, Keywords, Env) when is_list(Stx)->
    [Op|_] = Stx,
    case is_atom(Op) andalso lists:any(fun(KW) -> KW == Op end, Keywords) of
        false -> lists:map(fun(Expr) -> expand(Expr, Keywords, Env) end, Stx);
        true ->
            {transformer, Tx} = bs_env:lookup(Op, Env),
            expand(bs_compile:expand(Tx, Stx, Env), Stx, Keywords, Env)
    end;

expand(X, _, _, _) -> X.

%% tests %%

eval(S) ->
    Env = bs_scheme:env(),
    Exprs = bs_read:read_all(S),
    Result = lists:foldl(fun(Expr, _) -> bs_compile:eval(Expr, Env) end, nil, Exprs),
    Result.

-define(assertSchemeEqual(EXPECTED, ACTUAL),
        ?assertEqual(?TRUE, eval("(equal? " ++ ACTUAL ++ " " ++ EXPECTED ++ ")"))).
-define(assertSchemeTrue(ACTUAL), ?assertEqual(?TRUE, eval(ACTUAL))).
-define(assertSchemeFalse(ACTUAL), ?assertEqual(?FALSE, eval(ACTUAL))).
-define(assertSchemeException(EX_PATTERN, CODE), ?assertError(EX_PATTERN, eval(CODE))).

-define(_assertSchemeEqual(EXPECTED, ACTUAL), fun() -> ?assertSchemeEqual(EXPECTED, ACTUAL) end).
-define(_assertSchemeTrue(ACTUAL), fun() -> ?assertSchemeTrue(ACTUAL) end).
-define(_assertSchemeFalse(ACTUAL), fun() -> ?assertSchemeFalse(ACTUAL) end).
-define(_assertSchemeException(EX_PATTERN, CODE), fun() -> ?assertSchemeException(EX_PATTERN, CODE) end).


list_test_() ->
    [
     ?_assertSchemeEqual("'()", "(list)"),
     ?_assertSchemeEqual("'(1)", "(list 1)"),
     ?_assertSchemeEqual("'(1 2)", "(list 1 2)"),
     ?_assertSchemeEqual("'(a b c d e f)", "(list 'a 'b 'c 'd 'e 'f)"),
     ?_assertSchemeEqual("'(1 (2 3) 4)", "(list 1 (list 2 3) 4)")
    ].

listp_test_() ->
    [
     ?_assertSchemeTrue("(list? '())"),
     ?_assertSchemeTrue("(list? '(1))"),
     ?_assertSchemeFalse("(list? \"hello\")"),
     ?_assertSchemeFalse("(list? (cons 1 2))"),
     ?_assertSchemeFalse("(list? 1)"),
     %?_assertSchemeFalse("(list? 3.14)"),
     ?_assertSchemeFalse("(list? #t)"),
     ?_assertSchemeFalse("(list? #f)")
     %?_assertSchemeFalse("(list? #(1 2))")
    ].

list_ref_test_() ->
    [
     ?_assertSchemeEqual("'a", "(list-ref '(a b c) 0)"),
     ?_assertSchemeEqual("'c", "(list-ref '(a b c) 2)"),
     ?_assertSchemeEqual("'(bar)", "(list-ref '((foo) (bar)) 1)"),
     ?_assertSchemeException(function_clause, "(list-ref '() 0)")
    ].

str(S) -> "\"" ++ S ++ "\"".
    
pairp_test_() ->
    [
     ?_assertSchemeTrue("(pair? '(1))"),
     ?_assertSchemeTrue("(pair? '(1 2 3))"),
     ?_assertSchemeTrue("(pair? (cons 1 2))"),
     ?_assertSchemeFalse("(pair? '())"),
     ?_assertSchemeFalse("(pair? #t)"),
     ?_assertSchemeFalse("(pair? \"hello\")"),
     ?_assertSchemeFalse("(pair? 1)")
    ].
     

