%% Copyright
-module(tgws_root_sup).
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

%% game event manager

	GameControllerChildSpec = {{local, tgws_game_controller},
		{tgws_game_controller, start_link, []},
		permanent, infinity, worker, [tgws_game_controller]},
	{ok, {{one_for_one, 5, 10}, [
		GameControllerChildSpec
	]}}.
