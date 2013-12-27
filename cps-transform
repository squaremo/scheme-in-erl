#! /usr/bin/env escript
%% -*- erlang -*-

main([Expr]) ->
    Terms = grammar:parse(Expr),
    Prog = cps:program('k', Terms),
    io:put_chars(pp:pp(Prog)), io:nl().
