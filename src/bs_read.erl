-module(bs_read).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("bs_macros.hrl").

read(CharStream) -> parse(evaluate(tokenize(CharStream))).

read1(String) when is_list(String) ->
    CharStream = stream:from_list(String),
    Result = read1(CharStream),
    stream:destroy(CharStream),
    Result;

read1(CharStream) ->
    stream:next(read(CharStream)).


%%% TOKENIZE %%%

tokenize(CharStream) ->
    stream:make(fun() -> gen_tokenize(CharStream) end).

gen_tokenize(CharStream) -> gen_tokenize(CharStream, []).
gen_tokenize(CharStream, CurrentToken) ->
    StreamDone = stream:done(),
    OnWhitespace = fun() -> yield_token(CurrentToken), gen_tokenize(CharStream) end,
    case stream:next(CharStream) of
        StreamDone ->
            yield_token(CurrentToken),
            ok;
        Char ->
            case Char of
                $( ->
                    yield_token(CurrentToken),
                    yield_token("("),
                    gen_tokenize(CharStream);
                $) ->
                    yield_token(CurrentToken),
                    yield_token(")"),
                    gen_tokenize(CharStream);
                $   -> OnWhitespace();
                $\n -> OnWhitespace();
                $\r -> OnWhitespace();
                _ ->
                    gen_tokenize(CharStream, [Char|CurrentToken])
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


%%% PARSE %%%

parse(TokenStream) -> stream:make(fun() -> gen_parse(TokenStream) end).

gen_parse(TokenStream) ->
    stream:yield(parse1(TokenStream)),
    gen_parse(TokenStream).

parse1(TokenStream) -> parse1(TokenStream, none, []).
parse1(TokenStream, Opener, Acc) ->
    StreamDone = stream:done(),
    case stream:next(TokenStream) of
        StreamDone ->
            validate_closer(Opener, none),
            lists:reverse(Acc);
        '(' ->
            parse1(TokenStream, Opener, [parse1(TokenStream, '(', [])|Acc]);
        ')' ->
            validate_closer(Opener, ')'),
            lists:reverse(Acc);
        T ->
            parse1(TokenStream, Opener, [T|Acc])
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

str(List) -> stream:from_list(List).
toks(String) ->
    CharStream = stream:from_list(String),
    Result = stream:to_list(tokenize(CharStream)),
    stream:destroy(CharStream),
    Result.

tokenize_test_() ->
   [
     ?_assertEqual([], toks("")),
     ?_assertEqual(["0"], toks("0")),
     ?_assertEqual(["01"], toks("01")),
     ?_assertEqual(["0", "1"], toks("0 1")),
     ?_assertEqual(["01", "23"], toks("01 23")),
     ?_assertEqual(["01", "23"], toks("01\r\n23")),
     ?_assertEqual(["(", "foo", ")"], toks("(foo)"))
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
     ?_assertEqual([], read1("")),
     ?_assertEqual([foo], read1("foo")),
     ?_assertEqual([foo, bar], read1("foo bar")),
     ?_assertEqual([[]], read1("()")),
     ?_assertEqual([[foo]], read1("(foo)")),
     ?_assertEqual([a, [b, c], d], read1("a (b c) d")),
     ?_assertEqual([a, [b, [x, y, z], c], d], read1("a (b (x y z) c) d")),
     ?_assertError({mismatched, ')'}, read1(")")),
     ?_assertError({mismatched, ')'}, read1("x)")),
     ?_assertError({mismatched, '('}, read1("(")),
     ?_assertError({mismatched, '('}, read1("(x"))
    ].

read_test() ->
    ?assertEqual([[[lambda, [a, b], a], 1, 2]], read1("((lambda (a b) a) 1 2)")).
