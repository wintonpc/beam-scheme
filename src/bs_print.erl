-module(bs_print).
-export([print/1, pretty/1]).
-include_lib("eunit/include/eunit.hrl").
-include("bs_const.hrl").

print(Exp) ->
    Prettified = pretty(Exp),
    case Prettified of
        "" -> ok;
        _ -> io:format("~s~n", [Prettified])
    end.

pretty(Exp) when is_binary(Exp) ->
    "\"" ++ escape_string(binary_to_list(Exp)) ++ "\"";

pretty(Exp) when is_list(Exp) ->
    "(" ++ string:join(lists:map(fun pretty/1, Exp), " ") ++ ")";

pretty({vector, Array}) ->
    "#" ++ pretty(bs_scheme:vector_to_list({vector, Array}));

pretty(Exp) when is_atom(Exp) -> atom_to_list(Exp);

pretty(Exp) when is_number(Exp) -> hd(io_lib:format("~p", [Exp]));

pretty(?TRUE) -> "#t";
pretty(?FALSE) -> "#f";
pretty(?VOID) -> "";
pretty({char, C}) -> "#\\" ++ [C];

pretty({closure, _, _, _}) -> "#<procedure>";

pretty({primop, _, Fun}) -> io_lib:format("~p", [Fun]).

escape_string(S) -> escape_string(S, []).
escape_string(S, Acc) ->
    case S of
        "" -> lists:reverse(Acc);
        [H|T] ->
            case H of
                $\\ -> escape_string(T, [$\\|[$\\|Acc]]);
                $"  -> escape_string(T, [$"|[$\\|Acc]]);
                _   -> escape_string(T, [H|Acc])
            end
    end.
            

pretty_test_() ->
    [
     ?_assertEqual("1", pretty(1)),
     ?_assertEqual("-1", pretty(-1)),
     ?_assertEqual("3.14", pretty(3.14)),
     ?_assertEqual("#t", pretty(?TRUE)),
     ?_assertEqual("#f", pretty(?FALSE)),
     ?_assertEqual("#\\x", pretty({char, $x})),
     ?_assertEqual("sym", pretty(sym)),
     ?_assertEqual("()", pretty([])),
     ?_assertEqual("(foo)", pretty([foo])),
     ?_assertEqual("(foo bar)", pretty([foo, bar])),
     ?_assertEqual("\"woman\"", pretty(<<"woman">>)),
     ?_assertEqual("\"hello \\\\ \\\"hi!\\\"\"", pretty(<<"hello \\ \"hi!\"">>)),
     ?_assertEqual("#(1 foo \"say \\\"hello\\\"\")",
                   pretty(bs_scheme:list_to_vector([1, foo, <<"say \"hello\"">>])))
    ].
