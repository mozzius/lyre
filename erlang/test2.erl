-module(test2).

-export([addone/0, main/0]).

addone() -> receive X -> X + 1, addone() end.

main() ->
    PID = spawn(test2, addone, []), PID ! 1, PID ! "1".
