-module(interp).

-export([prog/1, prog/2, primitives/0]).

prog(Prog) ->
    prog(Prog, []).

prog(Prog, Env) ->
    progn(Prog, Env).

%% Terms are one of:
%%  - a list of terms, representing an application
%%  - an atom, representing a variable reference
%%  - a literal string (as a binary), boolean true or false, or number

%% Values include those things quotable above, and processes.

%% TODO : reader syntax (# and ')

%% Lists
evaluate(['lambda', Args | Body], Env) ->
    abstraction(Args, Body, Env);
evaluate(['quote', Val], _Env) ->
    quote(Val);
evaluate(['begin' | Exprs], Env) ->
    progn(Exprs, Env);
evaluate(['if', Test, Yes], Env) ->
    evaluate(['if', Test, Yes, []], Env);
evaluate(['if', Test, Yes, No], Env) ->
    alternate(Test, Yes, No, Env);
evaluate(['spawn' | Body], Env) ->
    thread(Body, Env);
evaluate([Head | Args], Env) ->
    application(Head, Args, Env);

%% Constants
evaluate(Val, _Env) when is_boolean(Val);
                         is_number(Val);
                         is_binary(Val) ->
    quote(Val);

%% Atom
evaluate(Symbol, Env) when is_atom(Symbol) ->
    ref(Symbol, Env).

quote(Val) ->
     Val.

%% If then [else]
alternate(Test, IfTrue, IfFalse, Env) ->
    case evaluate(Test, Env) of
        true -> evaluate(IfTrue, Env);
        _    -> evaluate(IfFalse, Env)
    end.

%% A sequence of expressions
progn([], _Env) ->
    undefined;
progn([Last], Env) ->
    evaluate(Last, Env);
progn([H | T], Env) ->
    evaluate(H, Env),
    progn(T, Env).

%% Spawns a process to run the body. Unlike progn, returns the process
%% as a value, rather than the value of the last expression.
thread(Body, Env) ->
    spawn(fun () -> progn(Body, Env) end).

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

primitives() ->
    [{send, fun ([Receiver, Msg]) -> Receiver ! Msg end},
     {recv, fun ([]) -> receive Val -> Val end end},
     {out, fun ([Term]) -> io:format("~w\n", [Term]) end}].
