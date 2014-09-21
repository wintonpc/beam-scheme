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
    case stream:next(CharStream) of
        StreamDone ->
            yield_token(CurrentToken),
            ok;
        Char ->
            on_char(Char, CharStream, CurrentToken)
    end.

on_char(Char, CharStream, CurrentToken) ->
    OnWhitespace = fun() -> yield_token(CurrentToken), gen_tokenize(CharStream) end,
    Yield = fun(X) ->
                    yield_token(CurrentToken),
                    yield_token(lists:reverse(X)),
                    gen_tokenize(CharStream)
            end,
    
    case Char of
        $(  -> Yield([Char]);
        $)  -> Yield([Char]);
        $[  -> Yield([Char]);
        $]  -> Yield([Char]);
        $'  -> Yield([Char]);
        $`  -> Yield([Char]);
        $,  ->
            case stream:next(CharStream) of
                $@ -> Yield(",@");
                Char2 ->
                    yield_token([Char]),
                    on_char(Char2, CharStream, CurrentToken)
            end;
        $#  ->
            yield_token(CurrentToken),
            case stream:next(CharStream) of
                $t ->
                    yield_token(?TRUE),
                    gen_tokenize(CharStream);
                $f ->
                    yield_token(?FALSE),
                    gen_tokenize(CharStream);
                $; ->
                    Yield("#;");
                $\\ ->
                    case stream:next(CharStream) of
                        LitChar ->
                            yield_token({char, LitChar}),
                            gen_tokenize(CharStream)
                    end;
                $% -> gen_tokenize(CharStream, "%#");
                Char2 ->
                    yield_token([Char]),
                    on_char(Char2, CharStream, CurrentToken)
            end;
        $"  ->
            yield_token(CurrentToken),
            yield_token(list_to_binary(read_string_token(CharStream))),
            gen_tokenize(CharStream);
        $;  ->
            yield_token(CurrentToken),
            eat_until_newline(CharStream),
            gen_tokenize(CharStream);
        $   -> OnWhitespace();
        $\n -> OnWhitespace();
        $\r -> OnWhitespace();
        _ ->
            gen_tokenize(CharStream, [Char|CurrentToken])
    end.

read_string_token(CharStream) ->
    lists:reverse(read_string_token(CharStream, [])).

read_string_token(CharStream, CurrentToken) ->
    case stream:next(CharStream) of
        $\\ -> read_string_token(CharStream, [stream:next(CharStream)|CurrentToken]);
        $" -> CurrentToken;
        Char -> read_string_token(CharStream, [Char|CurrentToken])
    end.

yield_token([]) -> ok;
yield_token(List) when is_list(List) ->
    stream:yield(lists:reverse(List));
yield_token(T) -> stream:yield(T).

eat_until_newline(CharStream) ->
    StreamDone = stream:done(),
    case stream:next(CharStream) of
        StreamDone -> ok;
        $\n -> ok;
        _ -> eat_until_newline(CharStream)
    end.
    

%%% EVALUATE %%%

evaluate(TokenStream) ->
    stream:map(TokenStream, fun evaluate_token/1).

evaluate_token(T) ->
    cond_([
           ?WHEN(tok_is_integer(T)), ?THEN(string_to_integer(T)),
           ?WHEN(is_list(T)), ?THEN(list_to_atom(T))
          ], ?ELSE(T)).

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

gen_parse(TokenStream) -> gen_parse(TokenStream, none, fun(X, _) -> stream:yield(X) end, nil).

gen_parse(TokenStream, Opener, Add, Acc) ->
    StreamDone = stream:done(),
    Tok = stream:next(TokenStream),
    AddAndContinue = fun(X) ->
                             case Add of
                                 nil -> X;
                                 _ -> gen_parse(TokenStream, Opener, Add, Add(X, Acc))
                             end
                     end,
    NextExpr = fun() -> gen_parse(TokenStream, none, nil, nil) end,
    TransformNextAndContinue = fun(Transform) -> AddAndContinue(Transform(NextExpr())) end,
    %io:format(user, "gen_parse(~p ..., ~p, ~p, ~p)~n", [Tok, Opener, Add, Acc]),
    OpenList = fun(Opener2) -> AddAndContinue(gen_parse(TokenStream, Opener2, fun cons/2, [])) end,
    CloseList = fun(Closer) -> validate_closer(Opener, Closer), reverse_list(Acc, []) end,
    
    case Tok of
        StreamDone ->
            validate_closer(Opener, none),
            Acc;
        '(' -> OpenList(Tok);
        ')' -> CloseList(Tok);
        '[' -> OpenList(Tok);
        ']' -> CloseList(Tok);
        '.' ->
            Last = NextExpr(),
            Closer = stream:next(TokenStream),
            validate_closer(Opener, Closer),
            reverse_list(Acc, Last);
        '\'' ->
            TransformNextAndContinue(fun(X) -> [quote, X] end);
        '`' ->
            TransformNextAndContinue(fun(X) -> [quasiquote, X] end);
        ',' ->
            TransformNextAndContinue(fun(X) -> [unquote, X] end);
        ',@' ->
            TransformNextAndContinue(fun(X) -> ['unquote-splicing', X] end);
        '#' ->
            TransformNextAndContinue(fun bs_scheme:list_to_vector/1);
        '#;' ->
            _ = gen_parse(TokenStream, none, nil, nil),
            gen_parse(TokenStream, Opener, Add, Acc);
        T ->
            AddAndContinue(T)
    end.

nothing(_) -> ok.
identity(X) -> X.
cons(H, T) -> [H|T].

reverse_list([], Tail) -> Tail;
reverse_list([A|B], Tail) -> reverse_list(B, [A|Tail]).

yield_expr(Acc) ->
    stream:yield(lists:reverse(Acc)).
    
validate_closer(Opener, Closer) ->
    case {Opener, Closer} of
        {'(', ')'} -> ok;
        {'[', ']'} -> ok;
        {none, none} -> ok;
        {'(', none} -> error({mismatched, '('});
        {'[', none} -> error({mismatched, '['});
        {none, ')'} -> error({mismatched, ')'});
        {none, ']'} -> error({mismatched, ']'});
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
    ?_assertEqual(["'", "(", "1", "2", ")"], toks("'(1 2)")),
    ?_assertEqual([",@", "(", "+", "y", "z", ")"], toks(",@(+ y z)")),
    ?_assertEqual([<<"hello">>], toks("\"hello\"")),
    ?_assertEqual([<<"hello there">>], toks("\"hello there\"")),
    ?_assertEqual([<<"scheme says, \"hello there\"">>], toks("\"scheme says, \\\"hello there\\\"\"")),
    ?_assertEqual(["`", "(", "1", ",", "x", ",@", "(", "+", "y", "z", ")", ")"], toks("`(1 ,x ,@(+ y z))")),
    ?_assertEqual([?TRUE], toks("#t")),
    ?_assertEqual([?FALSE], toks("#f")),
    ?_assertEqual(["#", "(", "1", "2", ")"], toks("#(1 2)")),
    ?_assertEqual(["#;", "1"], toks("#;1")),
    ?_assertEqual(["#;", "1"], toks("#; 1")),
    ?_assertEqual(["#;", "(", "1", "2", ")"], toks("#;(1 2)")),
    ?_assertEqual(["(", "1", ".", "2", ")"], toks("(1 . 2)"))
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
     ?_assertEqual(?FALSE, evaluate_token(?FALSE)),
     ?_assertEqual(?TRUE, evaluate_token(?TRUE)),
     ?_assertEqual(<<"hello">>, evaluate_token(<<"hello">>)),
     ?_assertEqual({char, $x}, evaluate_token({char, $x}))
    ].

parse_test_() ->
    [
     
     ?_assertEqual(foo, read1("foo")),
     ?_assertEqual([foo, bar], read_all("foo bar")),
     ?_assertEqual([], read1("()")),
     ?_assertEqual([], read1("[]")),
     ?_assertEqual([foo], read1("(foo)")),
     ?_assertEqual(?FALSE, read1("#f")),
     ?_assertEqual(?TRUE, read1("#t")),
     ?_assertEqual({char, $x}, read1("#\\x")),
     ?_assertEqual([quote, [?TRUE]], read1("'(#t)")),
     ?_assertEqual([a, [b, c], d], read_all("a (b c) d")),
     ?_assertEqual([a, [b, [x, y, z], c], d], read_all("a (b (x y z) c) d")),
     ?_assertEqual([[quote, x], [quote, y]], read_all("'x 'y")),
     ?_assertEqual([[quote, [1, 2]], [quote, [3, 4]]], read_all("'(1 2) '(3 4)")),
     ?_assertEqual([], bs_scheme:vector_to_list(read1("#()"))),
     ?_assertEqual([foo], bs_scheme:vector_to_list(read1("#(foo)"))),
     ?_assertEqual([1, 2], bs_scheme:vector_to_list(read1("#(1 2)"))),
     ?_assertEqual([1, 3], read1("(1 #;2 3)")),
     ?_assertEqual([1|2], read1("(1 . 2)")),

     ?_assertEqual(ok, error_logger:tty(false)),
     ?_assertError({mismatched, ')'}, read_all(")")),
     ?_assertError({mismatched, ']'}, read_all("]")),
     ?_assertError({mismatched, ')'}, read_all("x)")),
     ?_assertError({mismatched, ']'}, read_all("x]")),
     ?_assertError({mismatched, '('}, read_all("(")),
     ?_assertError({mismatched, '['}, read_all("[")),
     ?_assertError({mismatched, '('}, read_all("(x")),
     ?_assertError({mismatched, '['}, read_all("[x")),
     ?_assertError({bad_closer, ']', for, '('}, read_all("(]")),
     ?_assertError({bad_closer, ')', for, '['}, read_all("[)")),
     ?_assertEqual(ok, error_logger:tty(true))
    ].

read_test() ->
    ?assertEqual([[[lambda, [a, b], a], 1, 2]], read_all("((lambda (a b) a) 1 2)")).

quote_test_() ->
    [
     ?_assertEqual([[quote, [1, 2]]], read_all("'(1 2)")),
     ?_assertEqual([[quote, [quote, [1, 2]]]], read_all("''(1 2)")),
     ?_assertEqual([[quote, [quote, [quote, [1, 2]]]]], read_all("'''(1 2)")),
     ?_assertEqual([[quote, [1, [quote, [2]], 3]]], read_all("'(1 '(2) 3)"))
    ].
