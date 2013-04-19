%% Copyright
-module(hs_match_manager_service).
-author("jvalduvieco").

-behaviour(gen_server).
-define(TABLE_NAME, match_store).
%% API
-export([start_link/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([new_match/1, get_match_handle/1]).

-record(state, {match_id = 0,
	game_setup = none
}).

%% gen_server callbacks
%% API
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% API
new_match(GameId) ->
	gen_server:call(?MODULE, {new_match, GameId}).
get_match_handle(MatchId) ->
	gen_server:call(?MODULE, {get_match_handle, MatchId}).


init(_Args) ->
	State = #state{game_setup = hs_config:get(game_setup)},
	ets:new(?TABLE_NAME,[set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
	{ok, State}.

handle_call({new_match, GameId}, _From, State) ->
	GameSetup = proplists:get_value(GameId, State#state.game_setup),
	{ok, MatchRefereePid} = hs_match_referee:start_link(GameSetup),
	ets:insert(?TABLE_NAME,{State#state.match_id, GameId, MatchRefereePid}),
	{reply, {ok, State#state.match_id, MatchRefereePid}, State#state{match_id = State#state.match_id + 1 }};
handle_call({get_match_handle, BinMatchId}, _From, State)->
	MatchId = list_to_integer(binary_to_list(BinMatchId)),
	MatchRefereePid = ets:lookup_element(?TABLE_NAME, MatchId, 3),
	{reply, {ok, MatchRefereePid}, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
