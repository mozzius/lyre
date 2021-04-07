-module(test2).

-export([plusone/0, main/0]).

plusone() -> receive X -> X + 1, plusone() end.

main() ->
    PID = spawn(test2, plusone, []), PID ! 1, PID ! "1".
