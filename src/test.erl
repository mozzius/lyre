-module(test).

-export([main/1]).

plusone(Z) -> Z + 1.

main(X) -> Y = fun (P) -> P + 1 end, Y(plusone(X)).
