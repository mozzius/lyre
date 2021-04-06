-module(test1).

-export([main/0]).

plusone(X) -> X + 1.

main() -> plusone(1), plusone("1").
