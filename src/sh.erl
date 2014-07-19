-module(sh).
-export([c/0]).

c() ->
    {ok, [{_,_,Props}]} = file:consult("bs.app.src"),
    Mods = proplists:get_value(modules, Props),
    lists:map(fun c:c/1, Mods).
