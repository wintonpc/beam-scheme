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
    "(" ++ pretty_list(Exp, " ") ++ ")";
pretty({pair, A, B}) ->
    "(" ++ pretty_list({pair, A, B}, " ") ++ ")";

pretty({vector, Array}) ->
    "#" ++ pretty(bs_scheme:vector_to_list({vector, Array}));

pretty(Exp) when is_atom(Exp) -> atom_to_list(Exp);

pretty(Exp) when is_number(Exp) -> lists:flatten(io_lib:format("~p", [Exp]));


pretty(?TRUE) -> "#t";
pretty(?FALSE) -> "#f";
pretty(?VOID) -> "";
pretty({char, C}) -> "#\\" ++ [C];

pretty({closure, _, _, _}) -> "#<procedure>";
pretty({transformer, _}) -> "#<syntax-transformer>";
pretty({hashtable, _}) -> "#<hashtable>";
pretty(Exp) when is_function(Exp) ->
    {name, Name} = erlang:fun_info(Exp, name),
    lists:flatten(io_lib:format("#<primitive procedure ~s>", [Name]));

pretty({values, Values}) ->
    pretty_list(Values, "\n");

pretty({primop, _, Fun}) -> lists:flatten(io_lib:format("~p", [Fun]));

pretty(X) -> lists:flatten(io_lib:format("#<native-object ~p>", [X])).

pretty_list([], _) -> "";
pretty_list([H], _) -> pretty(H);
pretty_list([H|T], Sep) when is_list(T) ->
    pretty_list_proper(H, T, Sep);
pretty_list([H|T], _) ->
    pretty_list_improper(H, T);
pretty_list({pair, HBox, TBox}, Sep) ->
    H = box:get(HBox),
    T = box:get(TBox),
    case T of
        [] -> pretty(H);
        {pair, _, _} ->  pretty_list_proper(H, T, Sep);
        _ -> pretty_list_improper(H, T)
    end.

pretty_list_proper(H, T, Sep) ->
    pretty(H) ++ Sep ++ pretty_list(T, Sep).

pretty_list_improper(H, T) ->
    pretty(H) ++ " . " ++ pretty(T).

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
            

pp(X) ->
    pretty(bs_scheme:eval(X)).

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
     ?_assertEqual("foo\nbar", pretty({values, [foo, bar]})),
     ?_assertEqual("(foo . bar)", pretty([foo|bar])),
     ?_assertEqual("(foo bar . baz)", pretty([foo,bar|baz])),
     ?_assertEqual("\"woman\"", pretty(<<"woman">>)),
     ?_assertEqual("\"hello \\\\ \\\"hi!\\\"\"", pretty(<<"hello \\ \"hi!\"">>)),
     ?_assertEqual("#(1 foo \"say \\\"hello\\\"\")",
                   pretty(bs_scheme:list_to_vector([1, foo, <<"say \"hello\"">>]))),
     ?_assertEqual("(1 . 2)", pp("(mcons 1 2)")),
     ?_assertEqual("(1 2 3)", pp("(mlist 1 2 3)")),
     ?_assertEqual("(1 2 . 3)", pp("(mcons 1 (mcons 2 3))"))
    ].
