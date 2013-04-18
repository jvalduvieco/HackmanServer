%% Copyright
-module(hs_match_referee).
-behaviour(gen_fsm).

%% API
-export([start_link/0]).
-export([join/2, start_match/1, new_player/3, get_objects/1, position_update/3, pick_object/3, list_players/1]).
-export([init/1, code_change/4, handle_info/3, handle_sync_event/4, handle_event/3, terminate/3]).
-export([waiting/2, waiting/3, playing/2, finished/2]).

-record(state, {
	map_store = none,
	player_store = none,
	match_hub_pid = none}).

%%TODO: Refactor to a gen_server
start_link() ->
	gen_fsm:start_link(hs_match_referee, [], []).

join(MatchHandle, SessionId) ->
	gen_fsm:sync_send_event(MatchHandle, {join, SessionId}).

start_match(MatchHandle) ->
	gen_fsm:send_event(MatchHandle, {start_match}).

new_player(MatchHandle, Session, PlayerData) ->
	gen_fsm:send_event(MatchHandle, {new_player, Session, PlayerData}).

list_players(MatchHandle) ->
	gen_fsm:sync_send_event(MatchHandle, {list_players}).

get_objects(MatchHandle) ->
	gen_fsm:sync_send_event(MatchHandle, {get_objects}).

position_update(MatchHandle, Session, Message) ->
	gen_fsm:send_event(MatchHandle, {position_update, Session, Message}).

pick_object(MatchHandle, Session, Message) ->
	gen_fsm:send_event(MatchHandle, {pick_object, Session, Message}).

%% FIXME create a new init to recover data from a crash
init([]) ->
	%Load all match related services
	{ok, MapStore} = hs_file_map_loader:load(hs_config:get(map_file)),
	{ok, PlayerStore} = hs_player_store:init(),
	{ok, MatchHubPid} = gen_event:start_link(),
	% FIXME: Add a supervised event handler
	ok = gen_event:add_handler(MatchHubPid, hs_rules_enforcement, []),
	% Add AI's
	create_ai( MapStore, PlayerStore, MatchHubPid),
	{ok, waiting,
		#state{map_store =MapStore, player_store=PlayerStore, match_hub_pid = MatchHubPid}}.

create_ai(MapStore, PlayerStore, MatchHub) ->
	{ok, SessionId, _UserId} = hs_account_service:login(),
	PlayerId = <<"GhostAI0">>,
	PlayerData = [{<<"sessionId">>, SessionId},{<<"playerId">>, PlayerId}, {<<"type">>, <<"ghostAI">>}] ,
	{ok, _AiPid} = hs_ai_ghost:start_link({SessionId, {32, 96}, MapStore, {20, 22}, PlayerData, PlayerStore, MatchHub}).

handle_sync_event(_Event, _From, _StateName, State) ->
	lager:debug("hs_game_controller: handle_sync_event.."),
	{stop, unimplemented, State}.
handle_event(_Event, _StateName, State) ->
	lager:debug("hs_game_controller: handle_event.."),
	{stop, unimplemented, State}.
code_change(_OldVsn, StateName, State, _Extra) ->
	lager:debug("hs_game_controller: code_change.."),
	{ok, StateName, State}.
handle_info(_Info, _StateName, State) ->
	lager:debug("hs_game_controller: handle_info.."),
	{stop, unimplemented, State}.
terminate(_Reason, _StateName, _State) ->
	lager:debug("hs_game_controller: terminating.."),
	ok.

%% Sync events
waiting({join, SessionId}, {Pid, _Tag}, State) ->
	Parameters = [{session_id, SessionId}, {websocket_pid , Pid}],
	% FIXME: link to the connection handler
	ok = gen_event:add_handler(State#state.match_hub_pid, hs_events_handler, Parameters),
	{reply, {session_id, SessionId} , waiting, State};
waiting({get_objects}, {_Pid, _Tag}, State) ->
	{ok, DotObjects} = hs_map_store:get_entities_by_type(State#state.map_store, <<"dots">>),
	{ok, MagicDotsObjects} = hs_map_store:get_entities_by_type(State#state.map_store, <<"magicdots">>),
	{reply, DotObjects ++ MagicDotsObjects , waiting, State};
waiting({list_players}, {_Pid, _Tag}, State ) ->
	PlayerStore = State#state.player_store,
	PlayerList = hs_player_store:list_players(PlayerStore),
	{reply, PlayerList,waiting, State}.

% Async events
waiting({new_player, ClientHandle, PlayerData}, State) ->
	PlayerStore = State#state.player_store,
	PlayerType = proplists:get_value(<<"type">>, PlayerData, none),
	hs_player_store:add_player(PlayerStore, hs_client_handle:get_session(ClientHandle), PlayerType, PlayerData),
	gen_event:notify(State#state.match_hub_pid, {new_player, ClientHandle, PlayerData}),
	{next_state, waiting, State};
waiting({start_match}, State) ->
	lager:debug("Starting game..."),
	gen_event:notify(State#state.match_hub_pid, {start_match}),
	{next_state, playing, State}.

playing({position_update, ClientHandle, PlayerData}, State) ->
	PlayerStore = State#state.player_store,
	% FIXME. Maybe this is too expensive. Rework client message format
	Position = proplists:get_value(<<"pos">>, PlayerData),
	PositionX = proplists:get_value(<<"x">>, Position),
	PositionY = proplists:get_value(<<"y">>, Position),
	hs_player_store:update_player_position(PlayerStore, hs_client_handle:get_session(ClientHandle), {PositionX, PositionY}),
	gen_event:notify(State#state.match_hub_pid, {position_update, ClientHandle, PlayerData}),
	{next_state, playing, State};
playing({pick_object, ClientHandle, Data}, State) ->
	gen_event:notify(State#state.match_hub_pid, {pick_object, ClientHandle, Data}),
	{next_state, playing, State};
playing({end_match, _Data}, State) ->
	{next_state, finished, State};
playing(_Message, State) ->
	{next_state, playing, State}.

finished({restart, _Data},State) ->
	{next_state, initializing, State};
finished(_Message, State) ->
	{next_state, finished, State}.