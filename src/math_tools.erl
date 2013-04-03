%% Copyright
-module(math_tools).
-author("jvalduvieco").

%% API
-export([floor/1,ceiling/1]).

floor(X) ->
	T = erlang:trunc(X),
	case (X - T) of
		Neg when Neg < 0 -> T - 1;
		Pos when Pos > 0 -> T;
		_ -> T
	end.

ceiling(X) ->
	T = erlang:trunc(X),
	case (X - T) of
		Neg when Neg < 0 -> T;
		Pos when Pos > 0 -> T + 1;
		_ -> T
	end.
