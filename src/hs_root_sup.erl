%% Copyright
-module(hs_root_sup).
-author("jvalduvieco").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).

%% API
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
init([]) ->
	GameControllerChildSpec = {{local, hs_game_controller},
		{hs_game_controller, start_link, []},
		permanent, infinity, worker, [hs_game_controller]},
	{ok, {{one_for_one, 5, 10}, [
		GameControllerChildSpec
	]}}.
