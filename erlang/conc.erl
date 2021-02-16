-module(conc).

-export([printer/0, start/0]).

printer() ->
    receive
      stop -> stop;
      Msg -> io:format("Received: ~p~n", [Msg]), printer()
    end.

start() ->
    PID = spawn(conc, printer, []),
    PID ! "hello",
    PID ! "world",
    PID ! stop.


