-module(standard).

-export([int/1, length/1, print/1, str/1]).

% func (x: any) -> string
str(true) -> "true";
str(false) -> "false";
str(N) when is_integer(N) -> integer_to_list(N);
str(S) -> S.

% func (x: any) -> int
int(true) -> "true";
int(false) -> "false";
int(N) when is_list(N) -> list_to_integer(N);
int(S) -> S.

% func (x: string) -> int
length(X) -> list:length(X).

% func (x: any)
print(S) -> io:printf("~p~n", [S]).
