-module(bs_scheme).
-export([env/0]).
-include("bs_const.hrl").

bool(true) -> ?TRUE;
bool(false) -> ?FALSE.

compose(F, G) ->
    fun(X) -> F(G(X)) end.
            

env() ->
    E = bs_env:empty(),
    Eval = fun(Code) -> bs_compile:eval(bs_read:read1(Code), E) end,
    bs_env:set(E, '+', fun erlang:'+'/2),
    bs_env:set(E, '-', fun erlang:'-'/2),
    bs_env:set(E, '*', fun erlang:'*'/2),
    bs_env:set(E, '=', fun(A, B) -> bool(A == B) end),
    bs_env:set(E, 'cons', fun(A, B) -> [A|B] end),
    bs_env:set(E, 'car', fun erlang:hd/1),
    bs_env:set(E, 'cdr', fun erlang:tl/1),
    bs_env:set(E, 'append-lists', fun lists:append/1),
    bs_env:set(E, 'list-ref', fun(List, N) -> lists:nth(N + 1, List) end),
    Eval("(set! list (lambda L L))"),
    E.
    
