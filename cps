#! /usr/bin/env escript
%% -*- erlang -*-

main([Expr]) ->
    Terms = grammar:parse(Expr),
    Prog = cps:program('k', Terms),
    io:put_chars(pp(Prog)), io:nl().

pp([]) ->
    "()";
pp([Head | Rest]) ->
    ["(", pp(Head), pp_rest(Rest), ")"];
pp(true) -> "#t";
pp(false) -> "#f";
pp(Bin) when is_binary(Bin) ->
    [$", binary_to_list(Bin), $"];
pp(Term) ->
    io_lib:write(Term).

pp_rest([]) ->
    "";
pp_rest([Head | Rest]) ->
    [" ", pp(Head), pp_rest(Rest)].
