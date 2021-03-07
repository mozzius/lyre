-module(test).

-export([main/1]).

main(X) ->
    case X of X -> io:printf("%s", [X]) end, X + 3.
