-module(primitives).

-compile(export_all).

'+'(K, A, B) ->
    K(A + B).

'>'(K, A, B) ->
    K(A > B).

%% L is (lambda (k) ...), which is really translated to
%% (lambda (k1 k) ...)
'call/cc'(K, L) ->
    L(K, fun (_K1, V) -> K(V) end).
