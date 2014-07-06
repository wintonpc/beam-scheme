-module(bs_env).
-include_lib("eunit/include/eunit.hrl").
-export([empty/0, lookup/2, extend/3]).

empty() ->
    {bs_env, [], [], nil}.

lookup(Var, nil) ->
    error({not_defined, Var});

lookup(Var, {bs_env, Vars, Vals, Parent}) ->
    case lookup_val(Var, Vars, Vals) of
        'BS_ENV_NO_VAL' ->
            lookup(Var, Parent);
        Val -> Val
    end.

lookup_val(_, [], []) -> 'BS_ENV_NO_VAL';
lookup_val(Var, [Var|_], [Val|_]) -> Val;
lookup_val(Var, [_|Vars], [_|Vals]) -> lookup_val(Var, Vars, Vals).

extend(Env, Vars, Vals) ->
    {bs_env, Vars, Vals, Env}.

lookup_empty_test() ->
    ?assertError({not_defined, x}, lookup(x, empty())).

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

     
