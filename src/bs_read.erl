-module(bs_read).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("bs_macros.hrl").


read(S) ->
    parse(evaluate(tokenize(S))).

%%% TOKENIZE %%%

tokenize(S) ->
    stream:make(fun() -> gen_tokenize(S, []) end).

gen_tokenize(S, CurrentToken) ->
    case S of
        [] ->
            yield_token(CurrentToken),
            ok;
        [C|Rest] ->
            case C of
                $( ->
                    yield_token(CurrentToken),
                    yield_token("("),
                    gen_tokenize(Rest, []);
                $) ->
                    yield_token(CurrentToken),
                    yield_token(")"),
                    gen_tokenize(Rest, []);
                $ ->
                    yield_token(CurrentToken),
                    gen_tokenize(Rest, []);
                _ ->
                    gen_tokenize(Rest, [C|CurrentToken])
            end
    end.

yield_token(T) ->
    case T of
        [] -> ok;
        _ -> stream:yield(lists:reverse(T))
    end.


%%% EVALUATE %%%

evaluate(TokenStream) ->
    stream:map(TokenStream, fun evaluate_token/1).

evaluate_token(T) ->
    cond_([
           ?WHEN(T == "("), ?THEN('('),
           ?WHEN(T == ")"), ?THEN(')'),
           ?WHEN(tok_is_integer(T)), ?THEN(string_to_integer(T))
          ], ?ELSE(list_to_atom(T))).

string_to_integer(S) ->
    {V, _} = string:to_integer(S),
    V.

tok_is_integer([$+|Rest]) -> tok_is_uinteger(Rest);
tok_is_integer([$-|Rest]) -> tok_is_uinteger(Rest);
tok_is_integer(T) -> tok_is_uinteger(T).

tok_is_uinteger("") -> false;
tok_is_uinteger([C]) -> is_digit(C);
tok_is_uinteger([C|Rest]) -> is_digit(C) andalso tok_is_integer(Rest);
tok_is_uinteger(_) -> false.
    
is_digit(C) -> lists:member(C, "0123456789").

%is_sign(C) ->
%    lists:member(C, "+-").

% tests


%%% PARSE %%%

parse(TokenStream) -> parse(TokenStream, none, []).
parse(TokenStream, Opener, Acc) ->
    Done = stream:done(),
    case stream:next(TokenStream) of
        Done ->
            validate_closer(Opener, none),
            lists:reverse(Acc);
        '(' ->
            parse(TokenStream, Opener, [parse(TokenStream, '(', [])|Acc]);
        ')' ->
            validate_closer(Opener, ')'),
            lists:reverse(Acc);
        T ->
            parse(TokenStream, Opener, [T|Acc])
    end.

validate_closer(Opener, Closer) ->
    case {Opener, Closer} of
        {'(', ')'} -> ok;
        {'(', none} -> error({mismatched, '('});
        {none, none} -> ok;
        {none, ')'} -> error({mismatched, ')'});
        _ -> error({bad_closer, Closer, for, Opener})
    end.
    

%%% TESTS %%%

lst(Stream) -> stream:to_list(Stream).
str(List) -> stream:from_list(List).
toks(S) -> evaluate(tokenize(S)).

tokenize_test_() ->
    [
     ?_assertEqual([], lst(tokenize(""))),
     ?_assertEqual(["0"], lst(tokenize("0"))),
     ?_assertEqual(["01"], lst(tokenize("01"))),
     ?_assertEqual(["0", "1"], lst(tokenize("0 1"))),
     ?_assertEqual(["01", "23"], lst(tokenize("01 23"))),
     ?_assertEqual(["(", "foo", ")"], lst(tokenize("(foo)")))
    ].

evaluate_test_() ->
    [
     ?_assertEqual(0, evaluate_token("0")),
     ?_assertEqual(999, evaluate_token("999")),
     ?_assertEqual(-1, evaluate_token("-1")),
     ?_assertEqual(1, evaluate_token("+1")),
     ?_assertEqual(999, evaluate_token("+999")),
     ?_assertEqual('(', evaluate_token("(")),
     ?_assertEqual(')', evaluate_token(")")),
     ?_assertEqual(foo, evaluate_token("foo"))
    ].

parse_test_() ->
    [
     ?_assertEqual([], parse(toks(""))),
     ?_assertEqual([foo], parse(toks("foo"))),
     ?_assertEqual([foo, bar], parse(toks("foo bar"))),
     ?_assertEqual([[]], parse(toks("()"))),
     ?_assertEqual([[foo]], parse(toks("(foo)"))),
     ?_assertEqual([a, [b, c], d], parse(toks("a (b c) d"))),
     ?_assertEqual([a, [b, [x, y, z], c], d], parse(toks("a (b (x y z) c) d"))),
     ?_assertError({mismatched, ')'}, parse(toks(")"))),
     ?_assertError({mismatched, ')'}, parse(toks("x)"))),
     ?_assertError({mismatched, '('}, parse(toks("("))),
     ?_assertError({mismatched, '('}, parse(toks("(x")))
    ].

read_test() ->
    ?assertEqual([[[lambda, [a, b], a], 1, 2]], read("((lambda (a b) a) 1 2)")).
