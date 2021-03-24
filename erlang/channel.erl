-module(channel).

-export([make/0, make/1, send/2, recv/1, close/1]).

make() -> spawn(channel, make, [[]]).

make([]) ->
    receive
      {recv, PID} -> make([PID])
    end;
make([X|Xs]) ->
    receive
      {send, Msg} -> X ! {self(), Msg}, make(Xs);
      {recv, PID} -> make([X|Xs] ++ [PID])
    end.

recv(Channel) ->
    Channel ! {recv, self()},
    receive {Channel, Msg} -> Msg end.

send(Channel, Msg) -> Channel ! {send, Msg}.
