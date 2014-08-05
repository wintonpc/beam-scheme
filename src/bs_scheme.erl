-module(bs_scheme).
-export([env/0]).

env() ->
    E = bs_env:empty(),
    bs_env:set(E, '+', fun erlang:'+'/2),
    bs_env:set(E, '-', fun erlang:'-'/2)
        .
    
