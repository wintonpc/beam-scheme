-module(bs_read).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("bs_macros.hrl").
-include("bs_const.hrl").

read(CharStream) -> parse(evaluate(tokenize(CharStream))).

read1(String) when is_list(String) ->
    CharStream = stream:from_list(String),
    Result = read1(CharStream),
    stream:destroy(CharStream),
    Result;

read1(CharStream) ->
    stream:next(read(CharStream)).

read_all(String) when is_list(String) ->
    stream:to_list(read(stream:from_list(String))).


%%% TOKENIZE %%%

tokenize(CharStream) ->
    stream:make(fun() -> gen_tokenize(CharStream) end).

gen_tokenize(CharStream) -> gen_tokenize(CharStream, []).
gen_tokenize(CharStream, CurrentToken) ->
    StreamDone = stream:done(),
    OnWhitespace = fun() -> yield_token(CurrentToken), gen_tokenize(CharStream) end,
    YieldChar = fun(Char) ->
                        yield_token(CurrentToken),
                        yield_token([Char]),
                        gen_tokenize(CharStream)
                end,
                        
    case stream:next(CharStream) of
        StreamDone ->
            yield_token(CurrentToken),
            ok;
        Char ->
            case Char of
                $(  -> YieldChar(Char);
                $)  -> YieldChar(Char);
                $'  -> YieldChar(Char);
                $`  -> YieldChar(Char);
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
           ?WHEN(T == "'"), ?THEN('\''),
           ?WHEN(T == "`"), ?THEN('\`'),
           ?WHEN(T == "#f"), ?THEN(?FALSE),
           ?WHEN(T == "#t"), ?THEN(?TRUE),
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

gen_parse(TokenStream) -> gen_parse(TokenStream, none, fun nothing/1, fun(X, _) -> stream:yield(X) end, nil).

gen_parse(TokenStream, Opener, Yield, Add, Acc) ->
    StreamDone = stream:done(),
    Tok = stream:next(TokenStream),
    AddAndContinue = fun(X) ->
                             case Add of
                                 nil -> Yield(X);
                                 _ -> gen_parse(TokenStream, Opener, Yield, Add, Add(X, Acc))
                             end
                     end,
    %io:format(user, "gen_parse(~p ..., ~p, ~p, ~p, ~p)~n", [Tok, Opener, Yield, Add, Acc]),
    case Tok of
        StreamDone ->
            validate_closer(Opener, none),
            Yield(Acc);
        '(' ->
            Parsed = gen_parse(TokenStream, '(', fun lists:reverse/1, fun cons/2, []),
            AddAndContinue(Parsed);
        ')' ->
            validate_closer(Opener, ')'),
            Yield(Acc);
        '\'' ->
            Parsed = gen_parse(TokenStream, none, fun identity/1, nil, nil),
            AddAndContinue([quote, Parsed]);
        '\`' ->
            Parsed = gen_parse(TokenStream, none, fun identity/1, nil, nil),
            AddAndContinue([quasiquote, Parsed]);
        T ->
            AddAndContinue(T)
    end.

nothing(_) -> ok.
identity(X) -> X.
cons(H, T) -> [H|T].

yield_expr(Acc) ->
    stream:yield(lists:reverse(Acc)).
    
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
     ?_assertEqual(["(", "foo", ")"], toks("(foo)")),
     ?_assertEqual(["'", "x"], toks("'x")),
     ?_assertEqual(["'", "(", "1", "2", ")"], toks("'(1 2)"))
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
     ?_assertEqual(foo, evaluate_token("foo")),
     ?_assertEqual(?FALSE, evaluate_token("#f")),
     ?_assertEqual(?TRUE, evaluate_token("#t"))
    ].

parse_test_() ->
    [
     ?_assertEqual([foo], read_all("foo")),
     ?_assertEqual([foo, bar], read_all("foo bar")),
     ?_assertEqual([[]], read_all("()")),
     ?_assertEqual([[foo]], read_all("(foo)")),
     ?_assertEqual([a, [b, c], d], read_all("a (b c) d")),
     ?_assertEqual([a, [b, [x, y, z], c], d], read_all("a (b (x y z) c) d")),
     ?_assertEqual([[quote, x], [quote, y]], read_all("'x 'y")),
     ?_assertEqual([[quote, [1, 2]], [quote, [3, 4]]], read_all("'(1 2) '(3 4)")),

     ?_assertError({mismatched, ')'}, read_all(")")),
     ?_assertError({mismatched, ')'}, read_all("x)")),
     ?_assertError({mismatched, '('}, read_all("(")),
     ?_assertError({mismatched, '('}, read_all("(x"))
    ].

read_test() ->
    ?assertEqual([[[lambda, [a, b], a], 1, 2]], read_all("((lambda (a b) a) 1 2)")).
