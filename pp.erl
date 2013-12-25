-module(pp).

-export([pp/1]).

pp(Bin) when is_binary(Bin) ->
    [$", binary_to_list(Bin), $"];
pp(true) -> "#t";
pp(false) -> "#f";
pp(Atom) when is_atom(Atom) ->
    [$', atom_to_list(Atom)];
pp([]) ->
    "()";
pp([Head | Tail]) ->
    ["(", pp(Head), pp_rest(Tail)];
pp(Num) when is_integer(Num) ->
    integer_to_list(Num);
pp(Num) when is_float(Num) ->
    float_to_list(Num).

pp_rest([]) ->
    ")";
pp_rest([Head | Tail]) ->
    [" ", pp(Head), pp_rest(Tail)].
