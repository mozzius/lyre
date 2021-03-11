-module(test).

-export([main/1]).

plusone(Z) -> Z + 1.

main(X) ->
    Y = fun (P) -> P + 1 end, Z = fun plusone/1, Y(Z(X)).
