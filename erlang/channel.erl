-module(channel).

-export([channel/0, channel/1, send/2, recv/1, close/1, test/0, test/1]).

channel() -> spawn(channel, channel, [[]]).

channel([]) ->
    receive
      close -> closed;
      {recv, PID} -> channel([PID])
    end;
channel([X|Xs]) ->
    receive
      close -> close;
      {send, Msg} -> X ! {self(), Msg}, channel(Xs);
      {recv, PID} -> channel([X|Xs] ++ [PID])  % bad practice
    end.

recv(Channel) ->
    Channel ! {recv, self()},
    receive {Channel, Msg} -> Msg end.

send(Channel, Msg) -> Channel ! {send, Msg}.

close(Channel) -> Channel ! close.

test() ->
    Channel = channel(),
    send(Channel, "hello"),
    timer:sleep(500),
    spawn(channel, test, [Channel]),
    timer:sleep(500),
    send(Channel, "world"),
    timer:sleep(500),
    close(Channel).

test(Channel) ->
    io:format("Recv: ~p~n", [recv(Channel)]),
    io:format("Recv: ~p~n", [recv(Channel)]).
