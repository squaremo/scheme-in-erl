-module(cps).

-compile(export_all).
-export([program/2]).

next_name(N) ->
    receive P ->
            NChars = integer_to_list(N),
            P ! "gensym." ++ NChars
    end,
    next_name(N + 1).

start() ->
    Namer = spawn(fun () -> next_name(1) end),
    put(namer, Namer).

newname() ->
    newname("").

newname(Prefix) when is_atom(Prefix) ->
    newname(atom_to_list(Prefix));
newname(Prefix) ->
    case get(namer) of
        undefined ->
            start(),
            newname(Prefix);
        P -> P ! self(),
             receive V ->
                     list_to_atom(Prefix ++ [$$ | V])
             end
    end.

program(K, Exprs) ->
    body(K, Exprs).

transform(K, ['lambda', Args | Body]) ->
    abstraction(K, Args, Body);
transform(K, ['quote', Val]) ->
    [K, ['quote', Val]];
transform(K, ['begin' | Exprs]) ->
    body(K, Exprs);
transform(K, ['if', Test, True]) ->
    alternate(K, Test, True, []);
transform(K, ['if', Test, True, False]) ->
    alternate(K, Test, True, False);
transform(K, [Head | Args]) ->
    application(K, Head, Args);
transform(K, Val) when is_boolean(Val);
                       is_number(Val);
                       is_binary(Val) ->
    const(K, Val);
transform(K, Symbol) when is_atom(Symbol) ->
    ref(K, Symbol).

%% (lambda (a b) (+ a b)) ->
%% (k (lambda (k1 a b) (k1 (+ a b))))
abstraction(K, Args, Body) ->
    K1 = newname(k),
    [K, ['lambda', [K1 | Args] | [body(K1, Body)]]].

const(K, List) when is_list(List) ->
    [K, ['quote', List]];
const(K, Val) ->
    [K, Val].

alternate(K, Test, True, False) ->
    V = newname(v),
    K1 = ['lambda', [V],
          ['if', V,
           transform(K, True),
           transform(K, False)]],
    transform(K1, Test).


application(K, Head, Args) ->
    application(K, Head, Args, []).

application(K, Head, [], ArgVars) ->
    F = newname(f),
    K1 = ['lambda', [F], [F, K | lists:reverse(ArgVars)]],
    transform(K1, Head);
application(K, Head, [Arg | Rest], ArgVars) ->
    ArgVar = newname(),
    K1 = ['lambda', [ArgVar],
          application(K, Head, Rest, [ArgVar | ArgVars])],
    transform(K1, Arg).

ref(K, Var) ->
    [K, Var].

%% (begin) -> (k undefined)
body(K, []) ->
    [K, undefined];
%% (begin expr) -> expr
body(K, [Last]) ->
    transform(K, Last);
%% (begin expr1 expr2) ->
%% (expr1 (lambda (v) (expr2 k)))
body(K, [Head | Rest]) ->
    K1 = ['lambda', ['_'], body(K, Rest)],
    transform(K1, Head).
