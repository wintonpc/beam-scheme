-module(bs_scheme).
-export([env/0]).
-include("bs_const.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

env() ->
    E = bs_env:empty(),
    Eval = fun(Code) -> bs_compile:eval(bs_read:read1(Code), E) end,
    GensymNumbers = stream:naturals(),
    bs_env:set(E, '+/2', fun erlang:'+'/2),
    bs_env:set(E, '-', fun erlang:'-'/2),
    bs_env:set(E, '*', fun erlang:'*'/2),
    bs_env:set(E, '=', fun equals/2),
    bs_env:set(E, 'eq?', fun eq_p/2),
    bs_env:set(E, 'equal?', fun equal_p/2),
    bs_env:set(E, 'string=?', fun equal_p/2),
    bs_env:set(E, 'cons', fun(A, B) -> [A|B] end),
    bs_env:set(E, 'car', fun erlang:hd/1),
    bs_env:set(E, 'cdr', fun erlang:tl/1),
    bs_env:set(E, 'null?', fun null_p/1),
    bs_env:set(E, 'list?', fun list_p/1),
    bs_env:set(E, 'list->string', fun list_to_string/1),
    bs_env:set(E, 'string->list', fun string_to_list/1),
    bs_env:set(E, 'symbol->string', fun symbol_to_string/1),
    bs_env:set(E, 'string->symbol', fun string_to_symbol/1),
    bs_env:set(E, 'substring', fun substring/3),
    bs_env:set(E, 'list->vector', fun list_to_vector/1),
    bs_env:set(E, 'vector->list', fun vector_to_list/1),
    bs_env:set(E, 'vector-length', fun vector_length/1),
    bs_env:set(E, 'vector-ref', fun vector_ref/2),
    bs_env:set(E, 'vector-set!', fun vector_set/3),
    bs_env:set(E, 'vector?', fun vector_p/1),
    bs_env:set(E, 'make-hashtable', fun make_hashtable/0),
    bs_env:set(E, 'hashtable?', fun hashtable_p/1),
    bs_env:set(E, 'hashtable-set!', fun hashtable_set/3),
    bs_env:set(E, 'hashtable-ref', fun hashtable_ref/3),
    bs_env:set(E, 'hashtable-delete!', fun hashtable_delete/2),
    bs_env:set(E, 'symbol?', fun symbol_p/1),
    bs_env:set(E, 'string?', fun string_p/1),
    bs_env:set(E, 'procedure?', fun procedure_p/1),
    bs_env:set(E, '#%string-append', fun string_append/1),
    bs_env:set(E, 'pair?', fun pair_p/1),
    bs_env:set(E, 'append-lists', fun lists:append/1),
    bs_env:set(E, 'length', fun erlang:length/1),
    bs_env:set(E, 'reverse', fun lists:reverse/1),
    bs_env:set(E, '#%gensym', fun(L) -> gensym(L, GensymNumbers) end),
    bs_env:set(E, '#%let-transformer', fun transform_let/1),
    bs_env:set(E, '#%define-transformer', fun transform_define/1),
    Eval("(define-syntax let #%let-transformer)"),
    bs_env:set(E, 'cwd', fun() -> {ok, Path} = file:get_cwd(), list_to_binary(Path) end),
    bs_env:set(E, 'list-ref', fun(List, N) -> lists:nth(N + 1, List) end),
    bs_env:set(E, 'load', fun(File) -> load(File, E) end),
    bs_env:set(E, 'assert', fun assert_true/1),
    bs_env:set(E, 'assert-true', fun assert_true/1),
    bs_env:set(E, 'assert-false', fun assert_false/1),
    bs_env:set(E, 'expand', fun(Expr, Keywords) -> expand(Expr, Keywords, E) end),
    bs_env:set(E, 'eval', fun(Expr) -> bs_compile:eval(Expr, E) end),

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

equals(A, B) -> bool_e2s(A == B).
eq_p(A, B) -> bool_e2s(A == B).

equal_p(A, B) -> bool_e2s(equal_p2(A, B)).

equal_p2({vector, A}, {vector, B}) ->
    equal_p2(vector_to_list({vector, A}), vector_to_list({vector, B}));
equal_p2([A|X], [B|Y]) ->
    equal_p2(A, B) andalso equal_p2(X, Y);
equal_p2(A, B) -> A == B.

null_p([]) -> ?TRUE;
null_p(_) -> ?FALSE.

list_p([]) -> ?TRUE;
list_p([_|X]) -> list_p(X);
list_p(_) -> ?FALSE.

pair_p([_|_]) -> ?TRUE;
pair_p(_) -> ?FALSE.

string_p(X) -> bool_e2s(is_binary(X)).
symbol_p(X) -> bool_e2s(is_atom(X)).

procedure_p({closure, _, _, _}) -> ?TRUE;
procedure_p({primop, _, _}) -> ?TRUE;
procedure_p(_) -> ?FALSE.

bool_e2s(true) -> ?TRUE;
bool_e2s(false) -> ?FALSE.

bool_s2e(?TRUE) -> true;
bool_s2e(?FALSE) -> false.

char_e2s(C) -> {char, C}.
char_s2e({char, C}) -> C.

string_e2s(S) -> list_to_binary(S).
string_s2e(S) -> binary_to_list(S).

list_to_string(Chars) ->
    string_e2s(lists:map(fun char_s2e/1, Chars)).

string_to_list(Str) ->
    lists:map(fun char_e2s/1, string_s2e(Str)).

string_append(Strings) ->
    string_e2s(string:join(lists:map(fun string_s2e/1, Strings), "")).

symbol_to_string(Sym) ->
    string_e2s(atom_to_list(Sym)).

string_to_symbol(Str) ->
    list_to_atom(string_s2e(Str)).

substring(Str, Start, End) ->
    string_e2s(string:substr(string_s2e(Str), Start + 1, End - Start)).

list_to_vector(List) ->
    Length = length(List),
    {vector, box:make(copy_list_to_array(List, array:new(Length), 0, Length))}.

copy_list_to_array([], Array, Index, Index) -> Array;
copy_list_to_array([H|T], Array, Index, Length) ->
    copy_list_to_array(T, array:set(Index, H, Array), Index + 1, Length).

vector_to_list({vector, Box}) ->
    array:to_list(box:get(Box)).

vector_length({vector, Box}) ->
    array:size(box:get(Box)).

vector_p({vector, _}) -> ?TRUE;
vector_p(_) -> ?FALSE.

vector_ref({vector, Box}, Index) ->
    array:get(Index, box:get(Box)).

vector_set({vector, Box}, Index, Value) ->
    box:set(Box, array:set(Index, Value, box:get(Box))),
    ?VOID.

make_hashtable() ->
    {hashtable, box:make(gb_trees:empty())}.

hashtable_p({hashtable, _}) -> ?TRUE;
hashtable_p(_) -> ?FALSE.

hashtable_set({hashtable, Box}, K, V) ->
    box:set(Box, gb_trees:enter(K, V, box:get(Box))),
    ?VOID.

hashtable_ref({hashtable, Box}, K, Default) ->
    case gb_trees:lookup(K, box:get(Box)) of
        none -> Default;
        {value, V} -> V
    end.    

hashtable_delete({hashtable, Box}, K) ->
    box:set(Box, gb_trees:delete_any(K, box:get(Box))),
    ?VOID.

first([X|_]) -> X.
second([_, X|_]) -> X.

transform_let([_, Bindings | Bodies]) ->
    Names = lists:map(fun first/1, Bindings),
    Exprs = lists:map(fun second/1, Bindings),
    [[lambda, Names | Bodies]|Exprs].

% transform_letrec([_, Bindings | Bodies]) ->
%     Names = lists:map(fun first/1, Bindings),
%     Exprs = lists:map(fun second/1, Bindings),
%     [let, lists:map(fun(Name) -> [Name, ?FALSE] end, Names),
%      [let, lists:map(fun(Expr) -> [gensym([], GensymNumbers), Expr] end, Exprs),
%       [
                             
                            

transform_define([_, Name, Expr]) when is_atom(Name) ->
    ['set!', Name, Expr];
transform_define([_, [Name|Formals] | Bodies]) ->
    ['set!', Name, [lambda, Formals|Bodies]].

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

assert_true(X) -> ?assert(?FALSE /= X).
assert_false(X) -> ?assert(?FALSE == X).

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
     
list_to_string_test_() ->
    [
     ?_assertSchemeEqual(str("abc"), "(list->string (list #\\a #\\b #\\c))"),
     ?_assertSchemeEqual(str("abc"), "(list->string '(#\\a #\\b #\\c))")
    ].

string_to_list_test_() ->
    [
     ?_assertSchemeEqual("'(#\\a #\\b #\\c)", "(string->list \"abc\")")
    ].

string_ctor_test_() ->
    [
     ?_assertSchemeEqual(str("abc"), "(string #\\a #\\b #\\c)")
    ].
    
stringp_test_() ->
    [
     ?_assertSchemeTrue("(string? \"abc\")"),
     ?_assertSchemeFalse("(string? 1)"),
     ?_assertSchemeFalse("(string? 'a)"),
     ?_assertSchemeFalse("(string? '(1 2))"),
     ?_assertSchemeFalse("(string? #t)")
    ].

string_append_test_() ->
    [
     ?_assertSchemeEqual(str("foobar"), "(string-append \"foo\" \"bar\")")
    ].

symbol_to_string_test_() ->
    [
     ?_assertSchemeEqual(str("foo"), "(symbol->string 'foo)")
    ].

eqp_test_() ->
    [
     ?_assertSchemeTrue("(eq? #f #f)"),
     ?_assertSchemeFalse("(eq? #f 'false)")
    ].

string_to_symbol_test_() ->
    [
     ?_assertSchemeEqual("'foo", "(string->symbol \"foo\")"),
     ?_assertSchemeEqual("'true", "(string->symbol \"true\")")
    ].

symbolp_test_() ->
    [
     ?_assertSchemeTrue("(symbol? 'foo)"),
     ?_assertSchemeFalse("(symbol? '())"),
     ?_assertSchemeFalse("(symbol? #t)"),
     ?_assertSchemeFalse("(symbol? \"hello\")")
    ].

vector_test_() ->
    [
     ?_assertSchemeEqual("#()", "(vector)"),
     ?_assertSchemeEqual("#(1 2 3)", "(vector 1 2 3)")
    ].

list_to_vector_test_() ->
    [
     ?_assertSchemeEqual("#()", "(list->vector '())"),
     ?_assertSchemeEqual("#(1 2 3)", "(list->vector '(1 2 3))"),
     ?_assertSchemeEqual("#(1 '(2) 3)", "(list->vector '(1 '(2) 3))")
    ].
    
vector_to_list_test_() ->
    [
     ?_assertSchemeEqual("'()", "(vector->list #())"),
     ?_assertSchemeEqual("'(1 2 3)", "(vector->list #(1 2 3))"),
     ?_assertSchemeEqual("'(1 #(2) 3)", "(vector->list #(1 #(2) 3))")
    ].

vector_length_test_() ->
    [
     ?_assertSchemeEqual("0", "(vector-length #())"),
     ?_assertSchemeEqual("3", "(vector-length #(1 2 3))")
    ].

vectorp_test_() ->
    [
     ?_assertSchemeTrue("(vector? #(x y))"),
     ?_assertSchemeTrue("(vector? (vector 1 2))"),
     ?_assertSchemeFalse("(vector? '(1 2))"),
     ?_assertSchemeFalse("(vector? \"hello\")")
    ].

vector_ref_test_() ->
    [
     ?_assertSchemeEqual("'x", "(vector-ref #(x y) 0)"),
     ?_assertSchemeEqual("'(a b)", "(vector-ref #(99 (a b)) 1)")
    ].

vector_set_test() ->
    ?assertSchemeEqual("'(2 99)",
                       "(begin"
                       "(set! v #(1 2 3))"
                       "(set! a (vector-ref v 1))"
                       "(vector-set! v 1 99)"
                       "(list a (vector-ref v 1)))").

or_test_() ->
    [
     ?_assertEqual(?FALSE, eval("(or)")),
     ?_assertEqual(?FALSE, eval("(or #f)")),
     ?_assertEqual(1, eval("(or 1)")),
     ?_assertEqual(1, eval("(or 1 2)")),
     ?_assertEqual(5, eval("(or #f #f #f #f 5)"))
    ].

and_test_() ->
    [
     ?_assertEqual(?TRUE, eval("(and)")),
     ?_assertEqual(?FALSE, eval("(and #f)")),
     ?_assertEqual(1, eval("(and 1)")),
     ?_assertEqual(?FALSE, eval("(and 1 #f)")),
     ?_assertEqual(2, eval("(and 1 2)"))
    ].

not_test_() ->
    [
     ?_assertEqual(?TRUE, eval("(not #f)")),
     ?_assertEqual(?FALSE, eval("(not #t)")),
     ?_assertEqual(?FALSE, eval("(not 1)"))
    ].

equals_test_() ->
    [
     ?_assertSchemeTrue("(= 1 1)"),
     ?_assertSchemeTrue("(= -1 -1)"),
     ?_assertSchemeFalse("(= 1 2)")
    ].

eq_test_() ->
    [
     ?_assertSchemeTrue("(eq? 'a 'a)"),
     ?_assertSchemeFalse("(eq? 'a 'b)"),
     ?_assertSchemeTrue("(eq? 1 1)"),
     ?_assertSchemeFalse("(eq? 1 2)"),
     ?_assertSchemeTrue("(eq? '() '())"),
     ?_assertSchemeTrue("(eq? '(1 2) '(1 2))")
    ].

string_equals_test_() ->
    [
     ?_assertSchemeTrue("(string=? \"foo\" \"foo\")"),
     ?_assertSchemeFalse("(string=? \"foo\" \"bar\")")
    ].

substring_test_() ->
    [
     ?_assertSchemeEqual(str(""), "(substring \"\" 0 0)"),
     ?_assertSchemeEqual(str("hel"), "(substring \"hello\" 0 3)"),
     ?_assertSchemeEqual(str("llo"), "(substring \"hello\" 2 5)")
    ].

procedurep_test_() ->
    [
     ?_assertSchemeTrue("(procedure? (lambda (x) x))"),
     ?_assertSchemeTrue("(procedure? eq?)"),
     ?_assertSchemeFalse("(procedure? 0)")
    ].

dotted_pair_test_() ->
    [
     ?_assertSchemeEqual("(cons 1 2)", "'(1 . 2)"),
     ?_assertSchemeEqual("'(1 2)", "'(1 . (2))")
    ].

improper_list_test_() ->
    [
     ?_assertSchemeEqual("(cons 1 (cons 2 3))", "'(1 2 . 3)"),
     ?_assertSchemeEqual("(cons 0 (cons (cons 1 (cons 2 3)) (cons 9 '())))", "'(0 (1 2 . 3) 9)")
    ].

dotted_lambda_test_() ->
    [
     ?_assertSchemeEqual("'(1 2 (3 4))", "((lambda (a b . c) (list a b c)) 1 2 3 4)")
    ].

hashtable_test() ->
    eval("(let ([h (make-hashtable)])"
         "(assert-true (hashtable? h))"
         "(assert-false (hashtable? 1))"
         "(hashtable-set! h 'x 5)"
         "(hashtable-set! h '(1 2) 8)"
         "(assert (eq? 5 (hashtable-ref h 'x 0)))"
         "(assert (eq? 8 (hashtable-ref h '(1 2) 0)))"
         "(assert (eq? 0 (hashtable-ref h 'z 0)))"
         "(hashtable-set! h 'x 9)"
         "(assert (eq? 9 (hashtable-ref h 'x 0)))"
         "(hashtable-delete! h 'x)"
         "(assert (eq? 0 (hashtable-ref h 'x 0)))"
         ")").

map_test_() ->
    [
     ?_assertSchemeEqual("'(1 4 9)", "(map (lambda (x) (* x x)) '(1 2 3))"),
     ?_assertSchemeEqual("'()", "(map (lambda (x) (* x x)) '())")
    ].

define_test_() ->
    [
     ?_assertSchemeEqual("5", "(begin (define x 5) x)"),
     ?_assertSchemeEqual("5", "(begin (define (five) 5) (five))"),
     ?_assertSchemeEqual("5", "(begin (define (add1 x) (+ x 1)) (add1 4))"),
     ?_assertSchemeEqual("'(1 2 3 4)", "(begin (define (zlist . xs) xs) (zlist 1 2 3 4))")
    ].

eval_test_() ->
    [
     ?_assertSchemeEqual("3", "(eval '(+ 1 2))")
    ].

plus_test_() ->
    [
     ?_assertSchemeEqual("0", "(+)"),
     ?_assertSchemeEqual("42", "(+ 42)"),
     ?_assertSchemeEqual("15", "(+ 1 2 3 4 5)")
    ].

apply_test_() ->
    [
     ?_assertSchemeEqual("3", "(apply + '(1 2))"),
     ?_assertSchemeEqual("15", "(apply + '(1 2 3 4 5))"),
     ?_assertSchemeEqual("'(a b c)", "(apply list '(a b c))"),
     ?_assertSchemeEqual("3", "(let ([a 1] [b 2]) (apply + (list a b)))")
    ].

letrec_test_() ->
    [
     ?_assertSchemeEqual("15",
                         "(begin"
                         "(letrec ([sum (lambda (x) (if (= x 0) 0 (+ x (sum (- x 1)))))])"
                         "(sum 5)))")
    ].

% append_test_() ->
%     [
%      ?_assertSchemeEqual("'(a b c)", "(append '(a b c) '())"),
%      ?_assertSchemeEqual("'(a b c)", "(append '() '(a b c))"),
%      ?_assertSchemeEqual("'(a b c d)", "(append '(a b) '(c d))"),
%      ?_assertSchemeEqual("'(a b . c)", "(append '(a b) 'c)")
%     ].

% named_let_test_() ->
%     [
%      ?_assertSchemeEqual("", "(let fact ([n 5]) (if (= n 0) 1 (* n (fact (- n 1)))))")
%     ].
