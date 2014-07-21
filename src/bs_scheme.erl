-module(bs_scheme).
-export([env/0]).

env() ->
    bs_env:make(
      ['+',
       '-'],
      [fun erlang:'+'/2,
       fun erlang:'-'/2],
      nil).
