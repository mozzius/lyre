-module(test).

-export([main/1, tow/3]).

tow(X, Y, Z) -> [X, Y, Z].

main(X) ->
    Y = X + 1, Z = spawn(test, tow, [Y, 2, 3]), Z ! "hi".
