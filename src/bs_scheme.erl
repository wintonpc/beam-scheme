-module(bs_scheme).
-export([env/0]).
-include("bs_const.hrl").

bool(true) -> ?TRUE;
bool(false) -> ?FALSE.

env() ->
    E = bs_env:empty(),
    Eval = fun(Code) -> bs_compile:eval(bs_read:read1(Code), E) end,
    GensymNumbers = stream:naturals(),
    bs_env:set(E, '+', fun erlang:'+'/2),
    bs_env:set(E, '-', fun erlang:'-'/2),
    bs_env:set(E, '*', fun erlang:'*'/2),
    bs_env:set(E, '=', fun(A, B) -> bool(A == B) end),
    bs_env:set(E, 'cons', fun(A, B) -> [A|B] end),
    bs_env:set(E, 'car', fun erlang:hd/1),
    bs_env:set(E, 'cdr', fun erlang:tl/1),
    bs_env:set(E, 'null?', fun null_p/1),
    bs_env:set(E, 'append-lists', fun lists:append/1),
    bs_env:set(E, 'length', fun erlang:length/1),
    bs_env:set(E, '#%gensym', fun(L) -> gensym(L, GensymNumbers) end),
    bs_env:set(E, '#%let-transformer', fun transform_let/1),
    Eval("(define-syntax let #%let-transformer)"),
    bs_env:set(E, 'cwd', fun() -> {ok, Path} = file:get_cwd(), list_to_binary(Path) end),
    bs_env:set(E, 'list-ref', fun(List, N) -> lists:nth(N + 1, List) end),
    bs_env:set(E, 'load', fun(File) -> load(File, E) end),
    bs_env:set(E, 'expand', fun(Expr, Keywords) -> expand(Expr, Keywords, E) end),

    % {SourceFile, _} = filename:find_src(bs_scheme, [{"ebin", "src"}]),
    % Dir = filename:dirname(SourceFile),
    % Scheme0 = Dir ++ "/" ++ "scheme0.bs",
    % io:format(user, "SourceFile: ~p~n", [SourceFile]),
    % io:format(user, "Scheme0: ~p~n", [Scheme0]),
    load("/home/pwinton/git/bs/src/scheme0.bs", E),

    E.
    

load(File, Env) ->
    %io:format(user, "in '~p', opening '~p'~n", [file:get_cwd(), File]),
    {ok, Content} = file:read_file(File),
    ExprStream = bs_read:read(stream:from_list(binary_to_list(Content))),
    eval_all(ExprStream, Env),
    ?VOID.

eval_all(ExprStream, Env) ->
    StreamDone = stream:done(),
    case stream:next(ExprStream) of
        StreamDone -> ok;
        Expr ->
            bs_print:print(bs_compile:eval(Expr, Env)),
            eval_all(ExprStream, Env)
    end.

% gensym() ->
%     {A, B, C} = erlang:now(),
%     %list_to_atom(lists:flatten(io_lib:format("gensym-~p-~p-~p-~p", [erlang:node(), A, B, C]))).
%     list_to_atom(lists:flatten(io_lib:format("gensym-~p-~p-~p", [A, B, C]))).

gensym([], Numbers) ->
    gensym([g], Numbers);
gensym([Name], Numbers) ->
    list_to_atom(lists:flatten(io_lib:format("^~p~p", [Name, stream:next(Numbers)]))).

null_p([]) -> ?TRUE;
null_p(_) -> ?FALSE.

first([X|_]) -> X.
second([_, X|_]) -> X.

transform_let([_, Bindings, Body]) ->
    Names = lists:map(fun first/1, Bindings),
    Exprs = lists:map(fun second/1, Bindings),
    [[lambda, Names, Body]|Exprs].

expand(Expr, Keywords, Env) -> expand(Expr, [], Keywords, Env).

expand(Last, Last, _, _) -> Last;
expand(Stx, _, Keywords, Env) when is_list(Stx)->
    [Op|_] = Stx,
    case is_atom(Op) andalso lists:any(fun(KW) -> KW == Op end, Keywords) of
        false -> lists:map(fun(Expr) -> expand(Expr, Keywords, Env) end, Stx);
        true ->
            {transformer, Tx} = bs_env:lookup(Op, Env),
            expand(bs_compile:expand(Tx, Stx, Env), Stx, Keywords, Env)
    end;

expand(X, _, _, _) -> X.

    
