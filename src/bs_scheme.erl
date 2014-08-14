-module(bs_scheme).
-export([env/0]).
-include("bs_const.hrl").

bool(true) -> ?TRUE;
bool(false) -> ?FALSE.

compose(F, G) ->
    fun(X) -> F(G(X)) end.
            

env() ->
    E = bs_env:empty(),
    bs_env:set(E, '+', fun erlang:'+'/2),
    bs_env:set(E, '-', fun erlang:'-'/2),
    bs_env:set(E, '*', fun erlang:'*'/2),
    bs_env:set(E, '=', fun(A, B) -> bool(A == B) end),
    E.
    
