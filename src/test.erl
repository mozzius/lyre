-module(test).

-export([main/1,tow/3]).

tow(X,Y,Z) -> [X|[Y|[Z|[]]]].

main(X) ->
    if X == 1 -> 1;
       X == 2 -> 2;
       true -> 0
    end.
