-module(standard).

-export([int/1, length/1, print/1, str/1, make/1, make/3, recv/1, send/2]).

% func any -> string
str(true) -> "true";
str(false) -> "false";
str(N) when is_integer(N) -> integer_to_list(N);
str(S) -> S.

% func any -> int
int(true) -> "true";
int(false) -> "false";
int(N) when is_list(N) -> list_to_integer(N);
int(S) -> S.

% func string -> int
length(X) -> list:length(X).

% func string
print(S) -> io:fwrite("~s~n", [S]).

make(Z) -> spawn(standard, make, [[], [], Z]).
make([], [], Z) ->
  receive
    {send, Msg, PID} ->
      case Z of
        0 -> make([], [{Msg, none}], Z);
        _ -> PID ! {ack, self()},
             make([], [{Msg, PID}], Z)
      end;
    {recv, PID} -> make([PID], [], Z)
  end;
make([X|Xs], [], Z) ->
  receive
    {send, Msg, PID} ->
      case Z of
        0 -> ok;
        _ -> PID ! {ack, self()}
      end,
      X ! {self(), Msg},
      make(Xs, [], Z);
    {recv, PID} -> make([X|Xs] ++ [PID], [], Z)
  end;
make([], [Y|Ys], Z) ->
  receive
    {send, Msg, PID} ->
      case (erlang:length(Ys) + 1) < Z of
        true -> PID ! {ack, self()},
                make([], [Y|Ys] ++[{Msg, none}], Z);
        false -> make([], [Y|Ys] ++[{Msg, PID}], Z)
      end;
    {recv, XPID} ->
      case Y of
        {Msg, none} -> XPID ! {self(), Msg};
        {Msg, YPID} -> YPID ! {ack, self()},
                       XPID ! {self(), Msg}
      end,
      make([], Ys, Z)
  end;
make([X|Xs], [Y|Ys], Z) ->
  case Y of
    {Msg, none} -> X ! {self(), Msg};
    {Msg, YPID} -> YPID ! {ack, self()},
                   X ! {self(), Msg}
  end,
  make(Xs, Ys, Z).


send(Channel, Msg) -> Channel ! {send, Msg, self()}, 
  receive {ack, Channel} -> ok end.

recv(Channel) ->
    Channel ! {recv, self()},
    receive {Channel, Msg} -> Msg end.

