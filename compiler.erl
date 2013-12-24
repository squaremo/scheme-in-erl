-module(compiler).

-export([compile/1, compile/2, program/2, translate/2]).

-define(COMPILED_MOD, '$compiled').

%% Make a module def out of a body (list of expressions).  In my case
%% a program corresponds to a module with one entry point, a thunk
%% called 'entry'.
program(Prog, Env) ->
    KArg = 'k',
    BodyCps = cps:program(KArg, Prog),
    Body = body([BodyCps], env_extend(Env, [KArg])),
    [{attribute, 0, module, ?COMPILED_MOD},
     {attribute, 0, export, [{enter, 1}]},
     {function, 0, enter, 1,
      [{clause, 0, [{var, 0, KArg}], [], Body}]}].

compile(Exprs) ->
    compile(Exprs, []).

compile(Exprs, Env) ->
    Forms = program(Exprs, Env),
    {ok, Mod, Bin} = compile:forms(Forms),
    code:load_binary(Mod, "stdin", Bin),
    fun Mod:enter/1.

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

body(Exprs, Env) ->
    [translate(E, Env) || E <- Exprs].

quote(Val) when is_integer(Val) ->
    {integer, 0, Val};
quote(Val) when is_float(Val) ->
    {float, 0, Val};
quote(Val) when is_atom(Val) ->
    {atom, 0, Val};
quote(Val) when is_binary(Val) ->
    {bin, 0, [{bin_element, 0, {string, 0, binary_to_list(Val)},
               default, default}]};
quote([]) ->
    {nil, 0};
quote([H|T]) ->
    {cons, 0, quote(H), quote(T)}.

%% If then [else]
alternate(Test, IfTrue, IfFalse, Env) ->
    {'case', 0, translate(Test, Env),
     [{clause, 0, [{atom, 0, true}], [], [translate(IfTrue, Env)]},
      {clause, 0, [{var, 0, '_'}], [], [translate(IfFalse, Env)]}]}.

%% A sequence of expressions
progn(Exprs, Env) ->
    {block, 0, body(Exprs, Env)}.

%% A reference in arg position (i.e., not as the head of an
%% expression)
ref(Var, Env) ->
    case find_var(Var, Env) of
        {local, Var} ->
            {var, 0, Var};
        _ ->
            case find_primitive(Var) of
                {prim, Mod, Fun, Arity} ->
                    {'fun', 0, {function, {atom, 0, Mod},
                                {atom, 0, Fun},
                                {integer, 0, Arity}}};
                _ -> {unknown, Var}
            end
    end.

%% An application of a function to arguments
application(Head, Args0, Env) when is_atom(Head) ->
    F = case find_var(Head, Env) of
            {local, Var} ->
                {var, 0, Var};
            %% no globals yet
            _ ->
                Arity = length(Args0),
                case find_primitive(Head) of
                    {prim, Mod, Fun, Arity} ->
                        {remote, 0,
                         {atom, 0, Mod},
                         {atom, 0, Fun}};
                    {prim, _Mod, _Fun, Ar1} ->
                        throw ({wrong_arity, Head, Arity, Ar1});
                    _ -> throw({unknown, Head})
                end
        end,
    {call, 0, F, [translate(A, Env) || A <- Args0]};

%% left-left-lambda i.e., a 'let' form
%% ((lambda (a b) (+ a b)) 1 2)
application(['lambda', Args | Body], Values, Env) ->
    case {length(Args), length(Values)} of
        {L, L} ->
            Assigns = [assign(A, V, Env) ||
                          {A, V} <- lists:zip(Args, Values)],
            {block, 0, Assigns ++ body(Body, env_extend(Env, Args))};
        {_A, _B} ->
            throw ({arity_mismatch, Args, Values})
    end;

application(Head, Args0, Env) ->
    F = translate(Head, Env),
    Args = [translate(A, Env) || A <- Args0],
    {call, 0, F, Args}.

assign(P, V, Env) ->
    {match, 0, {var, 0, P}, translate(V, Env)}.

%% A lambda abstraction
abstraction(Args, Body, Env) ->
    {'fun', 0,
     {clauses, [{clause, 0, [{var, 0, V} || V <- Args], [],
                 body(Body, env_extend(Env, Args))}]}}.

env_extend(Env, Names) ->
    Names ++ Env.

find_var(Var, Env) ->
    case lists:member(Var, Env) of
        true -> {local, Var};
        _ -> case find_primitive(Var) of
                 Res = {prim, _Mod, _Var, _Arity} -> Res;
                 _ -> {unknown, Var}
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
