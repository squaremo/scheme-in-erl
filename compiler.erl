-module(compiler).

-export([compile/1, compile/2, program/2, translate/2]).

-define(COMPILED_MOD, '$compiled').

-include_lib("compiler/src/core_parse.hrl").

%% Make a module def out of a body (list of expressions).  In my case
%% a program corresponds to a module with one entry point, a thunk
%% called 'entry'.
program(Prog, Env) ->
    KArg = '$topk',
    Body = cps:program(KArg, Prog),
    Enter = #c_fun{
      vars = [#c_var{ name = KArg }],
      body = translate(Body, env_extend(Env, [KArg]))
     },
    #c_module{ name = #c_literal{ val = ?COMPILED_MOD },
               exports = [#c_var{ name = {'enter', 1} }],
               attrs = [],
               defs = [{#c_var{ name = {'enter', 1} },
                        Enter}] }.

compile(Exprs) ->
    compile(Exprs, []).

compile(Exprs, Env) ->
    Forms = program(Exprs, Env),
    {ok, _, Bin} = compile:forms(Forms, [from_core,binary]),
    code:load_binary(?COMPILED_MOD, "stdin", Bin),
    fun ?COMPILED_MOD:enter/1.

%% Lists, either application or special forms
translate(['lambda', Args | Body], Env) ->
    abstraction(Args, Body, Env);
translate(['quote', Val], _Env) ->
    quote(Val);
translate(['begin' | Exprs], Env) ->
    progn(Exprs, Env);
translate(['if', Test, Yes], Env) ->
    alternate(Test, Yes, [], Env);
translate(['if', Test, Yes, No], Env) ->
    alternate(Test, Yes, No, Env);
translate([Head | Args], Env) ->
    application(Head, Args, Env);

%% Constants
translate(Val, _Env) when is_boolean(Val);
                          is_number(Val);
                          is_binary(Val) ->
    quote(Val);

%% Atom
translate(Symbol, Env) when is_atom(Symbol) ->
    ref(Symbol, Env).

body([], _Env) ->
    #c_literal{ val = undefined };
body([Expr], Env) ->
    translate(Expr, Env);
body([Expr | Exprs], Env) ->
    #c_seq{
      arg = translate(Expr, Env),
      body = body(Exprs, Env)
     }.

quote(Val) when is_integer(Val) ->
    #c_literal{ val = Val };
quote(Val) when is_float(Val) ->
    #c_literal{ val = Val };
quote(Val) when is_atom(Val) ->
    #c_literal{ val = Val };
quote(Val) when is_binary(Val) ->
    #c_literal{ val = Val };
quote(Val) when is_list(Val) ->
    #c_literal{ val = Val }.

%% If then [else]
alternate(Test, IfTrue, IfFalse, Env) ->
    #c_case{ arg = translate(Test, Env),
             clauses =
             [#c_clause{ pats = [#c_literal{ val = true }],
                         guard = #c_literal{ val = true }, %% ??
                         body = translate(IfTrue, Env) },
              #c_clause{ pats = [#c_literal{ val = false }],
                         guard = #c_literal{ val = true }, %% ??
                         body = translate(IfFalse, Env) }]}.

%% A sequence of expressions
progn(Exprs, Env) ->
    body(Exprs, Env).

%% A reference in arg position (i.e., not as the head of an
%% expression)
ref(Var, Env) ->
    case find_var(Var, Env) of
        {local, Var} ->
            #c_var{ name = Var };
        _ ->
            case find_primitive(Var) of
                {prim, Mod, Fun, Arity} ->
                    %% In let forms this could be avoided
                    #c_call{ module = #c_literal{ val = 'erlang' },
                             name = #c_literal{ val = 'make_fun' },
                             args = [#c_literal{ val = Mod },
                                     #c_literal{ val = Fun },
                                     #c_literal{ val = Arity}] };
                _ -> throw({unknown, Var})
            end
    end.

%% An application of a function to arguments
application(Head, Args0, Env) when is_atom(Head) ->
    Args = [translate(A, Env) || A <- Args0],
    case find_var(Head, Env) of
        {local, Var} ->
            #c_apply{ op = #c_var{ name = Var },
                      args = Args };
        %% no globals yet
        _ ->
            Arity = length(Args0),
            case find_primitive(Head) of
                {prim, Mod, Fun, Arity} ->
                    #c_call{ module = #c_literal{ val = Mod },
                             name = #c_literal{ val = Fun },
                             args = Args };
                {prim, _Mod, _Fun, Ar1} ->
                    throw ({wrong_arity, Head, Arity, Ar1});
                _ ->
                    throw({unknown, Head})
            end
    end;

%% left-left-lambda i.e., a 'let' form
%% ((lambda (a b) (+ a b)) 1 2)
application(['lambda', Args | Body], Values, Env) ->
    case {length(Args), length(Values)} of
        {L, L} -> #c_let{ vars = [#c_var{ name = A } || A <- Args],
                          arg = #c_values{
                            es = [translate(V, Env) || V <- Values] },
                          body = body(Body, env_extend(Env, Args)) };
        {_A, _B} ->
            throw ({arity_mismatch, Args, Values})
    end;

%% Erm, is this ever reached?
application(Head, Args0, Env) ->
    F = translate(Head, Env),
    Args = [translate(A, Env) || A <- Args0],
    #c_apply{ op = F, args = Args }.

%% A lambda abstraction
abstraction(Args, Body, Env) ->
    #c_fun{ vars = [#c_var{ name = A } || A <- Args],
            body = body(Body, env_extend(Args, Env)) }.

env_extend(Env, Names) ->
    Names ++ Env.

find_var(Var, Env) ->
    case lists:member(Var, Env) of
        true -> {local, Var};
        _ -> case find_primitive(Var) of
                 Res = {prim, _Mod, _Var, _Arity} -> Res;
                 _ -> throw({unknown, Var})
             end
    end.

%% Assumes all primitives have a single form (i.e., none are
%% multiple-arity)
find_primitive(Var) ->
    Funs = [Fun ||
               Fun = {V, _A} <- primitives:module_info(functions),
               V =/= module_info],
    case lists:keyfind(Var, 1, Funs) of
        {Var, Arity} -> {prim, primitives, Var, Arity};
        _    -> unknown
    end.
