-module(interp).

-export([prog/1, prog/2]).

prog(Prog) ->
    prog(Prog, []).

prog(Prog, Env) ->
    progn(Prog, Env).

%% Terms are one of:
%%  - a list of terms, representing an application
%%  - an atom, representing a variable reference
%%  - a literal string (as a binary), boolean true or false, or number

%% TODO : reader syntax (# and ')

%% Lists
evaluate(['lambda', Args | Body], Env) ->
    abstraction(Args, Body, Env);
evaluate(['quote', Val], _Env) ->
    quote(Val);
evaluate(['begin' | Exprs], Env) ->
    progn(Exprs, Env);
evaluate([Head | Args], Env) ->
    application(Head, Args, Env);

%% Atom
evaluate(Symbol, Env) when is_atom(Symbol) ->
    ref(Symbol, Env);

%% Constants
evaluate(Val, _Env) when is_boolean(Val);
                        is_number(Val);
                        is_binary(Val) ->
    quote(Val).

quote(Val) ->
     Val.

%% A sequence of expressions
progn([], _Env) ->
    undefined;
progn([Last], Env) ->
    evaluate(Last, Env);
progn([H | T], Env) ->
    evaluate(H, Env),
    progn(T, Env).

%% A reference to the environment
ref(Var, Env) ->
    env_get(Var, Env).

%% An application of a function to arguments
application(Head, Args0, Env) ->
    F = evaluate(Head, Env),
    Args = [evaluate(A, Env) || A <- Args0],
    F(Args).

%% A lambda abstraction
abstraction(Args, Body, Env) ->
    fun (Vals) ->
            progn(Body, env_extend(Env, Args, Vals))
    end.

env_get(Var, Env) ->
    proplists:get_value(Var, Env).

env_extend(Env, Names, Values) ->
    lists:zip(Names, Values) ++ Env.
