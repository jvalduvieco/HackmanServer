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
%% 	GameControllerChildSpec = {{local, hs_match_referee},
%% 		{hs_match_referee, start_link, []},
%% 		permanent, infinity, worker, [hs_match_referee]},
	AccountServiceChildSpec = {{local, hs_account_service},
		{hs_account_service, start_link, []},
		permanent, infinity, worker, [hs_account_service]},
	MatchManagerServiceChildSpec = {{local, hs_match_manager_service},
		{hs_match_manager_service, start_link, []},
		permanent, infinity, worker, [hs_match_manager_service]},
	{ok, {{one_for_one, 5, 10}, [
		AccountServiceChildSpec, MatchManagerServiceChildSpec
	]}}.
