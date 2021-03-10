-module(test).

-export([main/1]).

main(X) -> Y = fun (P) -> P + 1 end, Y(X).
