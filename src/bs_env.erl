-module(bs_env).
-include_lib("eunit/include/eunit.hrl").
-export([empty/0, try_lookup/2, lookup/2, extend/3, set/3, all_vars/1, dump/1]).

make(Vars, Vals, Parent) ->
    {bs_env, box:make(Vars), box:make(Vals), Parent}.

empty() -> make([], [], nil).

lookup(Var, Env) ->
    case try_lookup(Var, Env) of
        not_found ->
            error({undefined_identifier, Var});
        {found, Val} ->
            Val
    end.

try_lookup(_, nil) ->
    not_found;

% returns not_found or {found, Value}
try_lookup(Var, {bs_env, VarsBox, ValsBox, Parent}) ->
    Vars = box:get(VarsBox),
    Vals = box:get(ValsBox),
    case lookup_var(Var, Vars, Vals) of
        'BS_ENV_NO_VAL' ->
            try_lookup(Var, Parent);
        Val -> {found, Val}
    end.

lookup_var(_, [], []) -> 'BS_ENV_NO_VAL';
lookup_var(Var, [Var|_], [Val|_]) -> Val;
lookup_var(Var, [_|Vars], [_|Vals]) -> lookup_var(Var, Vars, Vals).

extend(Env, Vars, Vals) -> make(Vars, Vals, Env).

set({bs_env, VarsBox, ValsBox, Parent}, Var, NewVal) ->
    Vars = box:get(VarsBox),
    Vals = box:get(ValsBox),
    case {set_var(Vars, Vals, Var, NewVal), Parent} of
        {not_found, nil} -> box:set(VarsBox, [Var|Vars]),
                            box:set(ValsBox, [NewVal|Vals]),
                            ok;
        {not_found, _} -> set(Parent, Var, NewVal);
        {NewVals, _} -> box:set(ValsBox, NewVals),
                        ok
    end.

set_var([], [], _, _) -> not_found;
set_var([Var|_], [_|RestVals], Var, NewVal) -> [NewVal|RestVals];
set_var([_|Vars], [OldVal|Vals], Var, NewVal) ->
    case set_var(Vars, Vals, Var, NewVal) of
        not_found -> not_found;
        Result -> [OldVal|Result]
    end.

all_vars(nil) -> [];
all_vars({bs_env, VarsBox, _, Parent}) ->
    box:get(VarsBox) ++ all_vars(Parent).

dump(Env) -> lists:flatten(dump(Env, all_vars(Env))).

dump(_, []) -> "";
dump(Env, [Var|T]) ->
    io_lib:format("~s: ~s~n", [bs_print:pretty(Var), bs_print:pretty(lookup(Var, Env))]) ++ dump(Env, T).


%%%% TESTS %%%%

lookup_empty_test() ->
    ?assertError({undefined_identifier, x}, lookup(x, empty())).

lookup_test_() ->
    Env1 = extend(empty(), [a, b], [1, 2]),
    Env2 = extend(Env1, [c, d], [3, 4]),
    [
     ?_assertEqual(1, lookup(a, Env1)),
     ?_assertEqual(2, lookup(b, Env1)),

     ?_assertEqual(1, lookup(a, Env2)),
     ?_assertEqual(2, lookup(b, Env2)),

     ?_assertEqual(3, lookup(c, Env2)),
     ?_assertEqual(4, lookup(d, Env2))
    ].

try_lookup_test_() ->
    Env = extend(empty(), [a], [5]),
    [
     ?_assertEqual({found, 5}, try_lookup(a, Env)),
     ?_assertEqual(not_found, try_lookup(b, Env))
    ].

shadow_test_() ->
    Env1 = extend(empty(), [a, b], [1, 2]),
    Env2 = extend(Env1, [b, c], [3, 4]),
    [
     ?_assertEqual(1, lookup(a, Env2)),
     ?_assertEqual(3, lookup(b, Env2)),
     ?_assertEqual(4, lookup(c, Env2))
    ].

set_undefined_test_() ->
    Env1 = empty(),
    Env2 = extend(Env1, [], []),
    set(Env2, x, 5),
    [
     ?_assertEqual(5, lookup(x, Env1))
    ].

set_test_() ->
    Env1 = extend(empty(), [a, b], [1, 2]),
    Env2 = extend(Env1, [b, c], [3, 4]),
    set(Env2, a, 11),
    set(Env2, b, 33),
    set(Env2, c, 44),
    [
     ?_assertEqual(11, lookup(a, Env2)),
     ?_assertEqual(33, lookup(b, Env2)),
     ?_assertEqual(44, lookup(c, Env2)),

     ?_assertEqual(11, lookup(a, Env1)),
     ?_assertEqual(2, lookup(b, Env1))
    ].

all_vars_test() ->    
    Env1 = extend(empty(), [a, b], [1, 2]),
    Env2 = extend(Env1, [c, d], [3, 4]),
    ?assertEqual([c, d, a, b], all_vars(Env2)).
