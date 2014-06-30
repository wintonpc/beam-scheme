-module(bs_read).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("bs_macros.hrl").


read(S) ->
    evaluate(tokenize(S)).

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

tokenize_test_() ->
    [
     ?_assertEqual([], stream:to_list(tokenize(""))),
     ?_assertEqual(["0"], stream:to_list(tokenize("0"))),
     ?_assertEqual(["01"], stream:to_list(tokenize("01"))),
     ?_assertEqual(["0", "1"], stream:to_list(tokenize("0 1"))),
     ?_assertEqual(["01", "23"], stream:to_list(tokenize("01 23"))),
     ?_assertEqual(["(", "foo", ")"], stream:to_list(tokenize("(foo)")))
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
