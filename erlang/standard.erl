-module(standard).
-export([str/1, length/1]).

str(X) -> case X of
    true -> "true";
    false -> "false";
    N -> integer_to_list(N)
  end.

length(X) -> list:length(X).