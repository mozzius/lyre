-module(test).

-export([world/3]).

world(X, Y, Z) ->
    lists:map(fun (F) -> [F, F] end, [X, Y, Z]).
