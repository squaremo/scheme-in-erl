-module(pp).

-export([pp/1, pp/2]).

pp(Val) -> pp(Val, false).

%% Val, UseContractions
pp(Bin, _) when is_binary(Bin) ->
    [$", binary_to_list(Bin), $"];
pp(true, _) -> "#t";
pp(false, _) -> "#f";
pp(Atom, _) when is_atom(Atom) ->
    atom_to_list(Atom);
pp([], _) ->
    "()";
pp([quote, Value], true) ->
    [$', pp(Value)];
pp([Head | Tail], C) ->
    ["(", pp(Head, C), pp_rest(Tail, C)];
pp(Num, _) when is_integer(Num) ->
    integer_to_list(Num);
pp(Num, _) when is_float(Num) ->
    float_to_list(Num).

pp_rest([], _) ->
    ")";
pp_rest([Head | Tail], C) ->
    [" ", pp(Head, C), pp_rest(Tail, C)].
