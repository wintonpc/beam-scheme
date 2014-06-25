%-define(COND(T1, B1, Else), case T1 of true -> B1; _ -> Else end).
%-define(COND(T1, B1, T2, B2, Else), case T1 of true -> B1; _ -> ?COND(T2, B2, Else) end).
%-define(COND(T1, B1, T2, B2, T3, B3, Else), case T1 of true -> B1; _ -> ?COND(T2, B2, T3, B3, Else) end).
%-define(COND(T1, B1, T2, B2, T3, B3, T4, B4, Else), case T1 of true -> B1; _ -> ?COND(T2, B2, T3, B3, T4, B4, Else) end).
%-define(COND(T1, B1, T2, B2, T3, B3, T4, B4, T5, B5, Else), case T1 of true -> B1; _ -> ?COND(T2, B2, T3, B3, T4, B4, T5, B5, Else) end).
%-define(COND(T1, B1, T2, B2, T3, B3, T4, B4, T5, B5, T6, B6, Else), case T1 of true -> B1; _ -> ?COND(T2, B2, T3, B3, T4, B4, T5, B5, T6, B6, Else) end).

-define(THUNK(Body), fun() -> Body end).
-define(WHEN(Body), ?THUNK(Body)).
-define(THEN(Body), ?THUNK(Body)).
-define(ELSE(Body), ?THUNK(Body)).

cond_([], Else) -> Else();
cond_([T, B|Rest], Else) ->
    case T() of
        true -> B();
        _ -> cond_(Rest, Else)
    end.
