-module(bs_env).
-include_lib("eunit/include/eunit.hrl").
-export([empty/0, lookup/2, extend/3, set/3]).

make(Vars, Vals, Parent) ->
    {bs_env, Vars, box:make(Vals), Parent}.

empty() -> make([], [], nil).

lookup(Var, nil) ->
    error({undefined_identifier, Var});

lookup(Var, {bs_env, Vars, ValsBox, Parent}) ->
    Vals = box:get(ValsBox),
    case lookup_var(Var, Vars, Vals) of
        'BS_ENV_NO_VAL' ->
            lookup(Var, Parent);
        Val -> Val
    end.

lookup_var(_, [], []) -> 'BS_ENV_NO_VAL';
lookup_var(Var, [Var|_], [Val|_]) -> Val;
lookup_var(Var, [_|Vars], [_|Vals]) -> lookup_var(Var, Vars, Vals).

extend(Env, Vars, Vals) -> make(Vars, Vals, Env).

set(nil, Var, _) -> error({undefined_identifier, Var});

set({bs_env, Vars, ValsBox, Parent}, Var, NewVal) ->
    Vals = box:get(ValsBox),
    case set_var(Vars, Vals, Var, NewVal) of
        not_found -> set(Parent, Var, NewVal);
        NewVals -> box:set(ValsBox, NewVals),
                   ok
    end.

set_var([], [], _, _) -> not_found;
set_var([Var|_], [_|RestVals], Var, NewVal) -> [NewVal|RestVals];
set_var([_|Vars], [OldVal|Vals], Var, NewVal) ->
    case set_var(Vars, Vals, Var, NewVal) of
        not_found -> not_found;
        Result -> [OldVal|Result]
    end.

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

shadow_test_() ->
    Env1 = extend(empty(), [a, b], [1, 2]),
    Env2 = extend(Env1, [b, c], [3, 4]),
    [
     ?_assertEqual(1, lookup(a, Env2)),
     ?_assertEqual(3, lookup(b, Env2)),
     ?_assertEqual(4, lookup(c, Env2))
    ].

set_undefined_test() ->
    ?assertError({undefined_identifier, x}, set(empty(), x, 1)).

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
    
