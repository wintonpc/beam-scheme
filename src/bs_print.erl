-module(bs_print).
-export([print/1]).
-include_lib("eunit/include/eunit.hrl").

print(Exp) ->
    io:format("~s~n", [pretty(Exp)]).

pretty(Exp) when is_list(Exp) ->
    % IsString = io_lib:printable_list(Exp),
    % case IsString of
    %     true -> hd(io_lib:format("~p", [Exp]));
    %     false -> "(" ++ lists:map(fun pretty/1, Exp) ++ ")"
    % end;
    "(" ++ string:join(lists:map(fun pretty/1, Exp), " ") ++ ")";

pretty(Exp) when is_atom(Exp) -> atom_to_list(Exp);

pretty(Exp) when is_number(Exp) -> hd(io_lib:format("~p", [Exp]));

pretty({closure, _, _, _}) -> "#<procedure>";

pretty({primop, _, Fun}) -> io_lib:format("~p", [Fun]).


pretty_test_() ->
    [
     ?_assertEqual("1", pretty(1)),
     ?_assertEqual("-1", pretty(-1)),
     ?_assertEqual("3.14", pretty(3.14)),
     ?_assertEqual("sym", pretty(sym)),
     %?_assertEqual("\"\"", pretty("")),
     %?_assertEqual("\"a string\"", pretty("a string")),
     %?_assertEqual("\"say \\\"hello\\\"\"", pretty("say \"hello\"")),
     ?_assertEqual("()", pretty([])),
     ?_assertEqual("(foo)", pretty([foo])),
     ?_assertEqual("(foo bar)", pretty([foo, bar]))
    ].
