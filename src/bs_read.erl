-module(bs_read).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("bs_macros.hrl").


read(S) ->
    read_tokens(tokenize(S)).

tokenize(S) ->
    tokenize(S, [], []).

tokenize(S, T, Ts) ->
    io:format("tokenize(~s, ~s, ~s)~n", [S, T, Ts]),
    case length(S) of
        0 ->
            case length(T) of
                0 ->
                    lists:reverse(Ts);
                _ ->
                    push_token(T, Ts)
            end;
        _ ->
            [C|Rest] = S,
            case C of
                $  -> % space
                    tokenize(Rest, [], push_token(T, Ts));
                _ ->
                    tokenize(Rest, [C|T], Ts)
            end
    end.

push_token(T, Ts) ->
    lists:reverse([lists:reverse(T) | Ts]).

read_tokens(Ts) ->
    lists:map(fun read_token/1, Ts).

read_token(T) ->
    cond_([
           ?WHEN(tok_is_integer(T)), ?THEN(string_to_integer(T))
          ], ?ELSE(error)).

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
     ?_assertEqual([], tokenize("")),
     ?_assertEqual(["0"], tokenize("0")),
     ?_assertEqual(["01"], tokenize("01")),
     ?_assertEqual(["0", "1"], tokenize("0 1")),
     ?_assertEqual(["01", "23"], tokenize("01 23"))
    ].

read_token_test_() ->
    [
     ?_assertEqual(0, read_token("0")),
     ?_assertEqual(999, read_token("999")),
     ?_assertEqual(-1, read_token("-1")),
     ?_assertEqual(1, read_token("+1")),
     ?_assertEqual(999, read_token("+999"))
    ].
