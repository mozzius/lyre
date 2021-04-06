-module(standard).

-export([int/1, length/1, print/1, str/1, make/0, make/1, recv/1, send/2]).

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

make() -> spawn(standard, make, [[]]).

make([]) ->
    receive
      {recv, PID} -> make([PID])
    end;
make([X|Xs]) ->
    receive
      {send, Msg} -> X ! {self(), Msg}, make(Xs);
      {recv, PID} -> make([X|Xs] ++ [PID])
    end.

send(Channel, Msg) -> Channel ! {send, Msg}.

recv(Channel) ->
    Channel ! {recv, self()},
    receive {Channel, Msg} -> Msg end.

