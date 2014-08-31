-module(bs_compile).
-include_lib("eunit/include/eunit.hrl").
-export([compile/2, eval/1, eval/2, expand/3]).

eval(X) -> eval(X, bs_env:empty()).

eval(X, Env) ->
    Compiled = compile(X, Env),
    %io:format(user, "eval:~n==> ~s~n~p~n", [bs_print:pretty(X), Compiled]),
    Result = bs_vm:vm(Compiled, Env),
    %io:format(user, "<== ~s~n", [bs_print:pretty(Result)]),
    Result.

compile(X) -> compile(X, bs_env:empty()).

compile(X, Env) -> compile(X, {halt}, {Env, nil}).

compile(X, Next, {Env, VarRibs}) when is_atom(X) ->
    %io:format(user, "compiling a reference~n", []),
    Refer = fun() -> {refer, X, Next} end,
    
    case bs_env:try_lookup(X, Env) of
        not_found -> Refer();
        {found, Obj} ->
            case Obj of
                {transformer, Tx} ->
                    compile(expand(Tx, X, {Env, VarRibs}), Next, {Env, VarRibs});
                Thing ->
                    case {is_free_identifier(X, VarRibs), Thing} of
                        {true, Fun} when is_function(Fun) ->
                            %io:format(user, "compiling primop~n", []),
                            {constant, {primop, arity(Fun), Fun}, Next};
                        _ -> Refer()
                    end
            end;
        _ -> Refer()
    end;

compile([quote, Obj], Next, _) ->
    {constant, Obj, Next};

compile([quasiquote, Obj], Next, EnvInfo) ->
    QQ = expand_quasiquote([quasiquote, Obj]),
    %io:format(user, "QQ: ~s  ->  ~s~n", [bs_print:pretty([quasiquote, Obj]), bs_print:pretty(QQ)]),
    compile(QQ, Next, EnvInfo);

compile([lambda, Vars, Body], Next, {Env, VarRibs}) ->
    {close, Vars, compile(Body, {return}, {Env, add_rib(Vars, VarRibs)}), Next};

compile(['define-syntax', Keyword, Transformer], {halt}, {Env, _}) ->
    bs_env:set(Env, Keyword, {transformer, Transformer}),
    {halt};

compile(['if', Test, Then, Else], Next, EnvInfo) ->
    ThenC = compile(Then, Next, EnvInfo),
    ElseC = compile(Else, Next, EnvInfo),
    compile(Test, {test, ThenC, ElseC}, EnvInfo);

compile(['set!', Var, X], Next, EnvInfo) ->
    compile(X, {assign, Var, Next}, EnvInfo);

compile([Op|Args], Next, {Env, VarRibs}) ->
    EnvInfo = {Env, VarRibs},
    case is_atom(Op) andalso bs_env:try_lookup(Op, Env) of
        {found, {transformer, Tx}} ->
            %io:format(user, "encountered syntax: ~s~n", [bs_print:pretty([Op|Args])]),
            compile(expand(Tx, [Op|Args], EnvInfo), Next, EnvInfo);
        _ ->
            Compiled = compile_args(Args, compile(Op, {apply}, EnvInfo), EnvInfo),
            case is_tail(Next) of
                true -> Compiled;
                false -> {frame, Compiled, Next}
            end
    end;

compile(X, Next, _) ->
    {constant, X, Next}.

expand(Tx, Stx, {Env, _}) when Env /= bs_env ->
    expand(Tx, Stx, Env);
expand(Tx, Stx, Env) -> 
    Expanded = eval([Tx, [quote, Stx]], Env),
    %io:format(user, "expand: ~s  ->  ~s~n", [bs_print:pretty(Stx), bs_print:pretty(Expanded)]),
    Expanded.

expand_quasiquote([quasiquote, Obj]) when not is_list(Obj) ->
    [quote, Obj];
expand_quasiquote([quasiquote, Obj]) ->
    ['append-lists', [list|lists:map(fun expand_quasiquote_element/1, Obj)]].

expand_quasiquote_element([unquote, X]) ->
    [list, X];
expand_quasiquote_element(['unquote-splicing', X]) ->
    X;
expand_quasiquote_element(X) ->
    [list, expand_quasiquote([quasiquote, X])].

compile_args([], Compiled, _) -> Compiled;
compile_args([Arg|Rest], Compiled, EnvInfo) ->
    compile_args(Rest, compile(Arg, {argument, Compiled}, EnvInfo), EnvInfo).

is_tail({return}) -> true;
is_tail(_) -> false.

add_rib(L, VarRibs) when is_atom(L) ->
    {[L], VarRibs};
add_rib(Vars, VarRibs) ->
    {Vars, VarRibs}.

is_free_identifier(Id, VarRibs) ->
    not is_bound_identifier(Id, VarRibs).

is_bound_identifier(_, nil) ->
    false;
is_bound_identifier(Id, {Vars, Parent}) ->
    lists:member(Id, Vars) orelse is_bound_identifier(Id, Parent).
    
arity(Fun) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    Arity.

compile_symbol_test() ->
    ?assertEqual({refer, foo, {halt}}, compile(foo)).

compile_integer_test() ->
    ?assertEqual({constant, 1, {halt}}, compile(1)).

compile_quote_test() ->
    ?assertEqual({constant, [foo, bar], {halt}}, compile([quote, [foo, bar]])).

compile_lambda_test() ->
    ?assertEqual({close, [x], {refer, x, {return}}, {halt}}, compile([lambda, [x], x])).

compile_if_test() ->
    ?assertEqual({constant, 5, {test, {constant, 1, {halt}}, {constant, 2, {halt}}}}, compile(['if', 5, 1, 2])).

compile_set_test() ->
    ?assertEqual({constant, 5, {assign, x, {halt}}}, compile(['set!', x, 5])).

compile_application_test() ->
    ?assertEqual({frame,
                  {constant, 2,
                   {argument,
                    {constant, 1,
                     {argument,
                      {close, [a, b], {refer, a, {return}}, {apply}}}}}},
                  {halt}},
                 compile([[lambda, [a, b], a], 1, 2])).

is_free_identifier_test_() ->
    VarRibs = {[a, b], {[c, d], nil}},
    [
     ?_assert(false == is_free_identifier(a, VarRibs)),
     ?_assert(false == is_free_identifier(b, VarRibs)),
     ?_assert(false == is_free_identifier(c, VarRibs)),
     ?_assert(false == is_free_identifier(d, VarRibs)),
     ?_assert(true == is_free_identifier(e, VarRibs))
    ].

