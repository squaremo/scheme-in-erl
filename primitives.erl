-module(primitives).

-compile(export_all).

'+'(K, A, B) ->
    K(A + B).

'>'(K, A, B) ->
    K(A > B).
