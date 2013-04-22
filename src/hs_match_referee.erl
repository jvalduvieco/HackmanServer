%% Copyright
-module(hs_match_referee).
-behaviour(gen_fsm).

%% API
-export([start_link/1]).
-export([join/2, start_match/1, end_match/1, new_player/3, get_objects/1, position_update/3, pick_object/3, list_players/1, player_left/2]).
-export([init/1, code_change/4, handle_info/3, handle_sync_event/4, handle_event/3, terminate/3]).
-export([waiting/2, playing/2]).

-record(state, {
	map_store = none,
	match_id = none,
	player_store = none,
	match_hub_pid = none,
	end_match_timer = none,
	start_match_timer = none,
	match_params = none,
	ai_pids = none}).

%%TODO: Refactor to a gen_server
start_link(Params) ->
	gen_fsm:start_link(hs_match_referee, Params, []).

join(MatchHandle, SessionId) ->
	gen_fsm:send_all_state_event(MatchHandle, {join, SessionId, self()}).

start_match(MatchHandle) ->
	gen_fsm:send_event(MatchHandle, {start_match}).
end_match(MatchHandle) ->
	gen_fsm:send_event(MatchHandle, {start_match}).

new_player(MatchHandle, Session, PlayerData) ->
	gen_fsm:send_all_state_event(MatchHandle, {new_player, Session, PlayerData}).

player_left(MatchHandle, ClientHandle) ->
	gen_fsm:send_all_state_event(MatchHandle, {player_left, ClientHandle}).

list_players(MatchHandle) ->
	gen_fsm:sync_send_all_state_event(MatchHandle, {list_players}).

get_objects(MatchHandle) ->
	gen_fsm:sync_send_all_state_event(MatchHandle, {get_objects}).

position_update(MatchHandle, Session, Message) ->
	gen_fsm:send_event(MatchHandle, {position_update, Session, Message}).

pick_object(MatchHandle, Session, Message) ->
	gen_fsm:send_event(MatchHandle, {pick_object, Session, Message}).

%% FIXME create a new init to recover data from a crash
init({MatchId, MatchParams}) ->
	%Load all match related services
	{ok, MapStore} = hs_file_map_loader:load(proplists:get_value(map_file, MatchParams, none)),
	{ok, PlayerStore} = hs_player_store:init(),
	{ok, MatchHubPid} = gen_event:start_link(),
	% FIXME: Add a supervised event handler
	StartMatchTimer = gen_fsm:start_timer(proplists:get_value(match_duration, MatchParams, none), start_match_timeout),
	ok = gen_event:add_handler(MatchHubPid, {hs_rules_enforcement, MatchId}, {MatchParams, PlayerStore}),
	% Add AI's
	AIPids = wake_ais(MapStore, PlayerStore, MatchHubPid, MatchParams),
	{ok, waiting,
		#state{map_store = MapStore, player_store = PlayerStore, match_hub_pid = MatchHubPid,
			start_match_timer = StartMatchTimer, match_params = MatchParams, match_id = MatchId, ai_pids = AIPids}}.
wake_ai(MapStore, PlayerStore, MatchHub, AIParams) ->
	AIModule = proplists:get_value(ai_module, AIParams),
	{ok, AiPid} = AIModule:start_link({MapStore, PlayerStore, MatchHub, AIParams}),
	AiPid.
wake_ais(MapStore, PlayerStore, MatchHub, MatchParams) ->
	AIDef = proplists:get_value(match_ai, MatchParams, none),
	[wake_ai(MapStore, PlayerStore, MatchHub, AIParams)||{_Id, AIParams} <- AIDef].

code_change(_OldVsn, StateName, State, _Extra) ->
	lager:debug("hs_game_controller: code_change.."),
	{ok, StateName, State}.
handle_info(_Info, _StateName, State) ->
	lager:debug("hs_game_controller: handle_info.."),
	{stop, unimplemented, State}.
terminate(_Reason, _StateName, _State) ->
	lager:debug("hs_game_controller: terminating.."),
	ok.

do_start_match(State) ->
	lager:info("ref: Start match [~p]", [State#state.match_id]),
	gen_event:notify(State#state.match_hub_pid, {start_match}),
	MatchDuration = proplists:get_value(match_duration, State#state.match_params, none),
	EndMatchTimer = gen_fsm:start_timer(MatchDuration, end_match_timeout),
	{next_state, playing, State#state{end_match_timer = EndMatchTimer}}.

waiting({start_match}, State) ->
	do_start_match(State);
waiting({timeout, _Ref, start_match_timeout}, State) ->
	do_start_match(State);
waiting(_Event, State) ->
	{next_state, waiting, State}.

do_end_match(State) ->
	lager:info("ref: End match [~p]", [State#state.match_id]),
	PauseDuration = proplists:get_value(match_pause, State#state.match_params, none),
	StartMatchTimer = gen_fsm:start_timer(PauseDuration, start_match_timeout),
	gen_event:notify(State#state.match_hub_pid, {end_match}),
	{next_state, waiting, State#state{start_match_timer = StartMatchTimer}}.

playing({position_update, ClientHandle, PlayerData}, State) ->
	PlayerStore = State#state.player_store,
	gen_event:notify(State#state.match_hub_pid, {position_update, ClientHandle, PlayerData}),
	% FIXME. Maybe this is too expensive. Rework client message format
	Position = proplists:get_value(<<"pos">>, PlayerData),
	PositionX = proplists:get_value(<<"x">>, Position),
	PositionY = proplists:get_value(<<"y">>, Position),
	hs_player_store:update_player_position(PlayerStore, hs_client_handle:get_session(ClientHandle), {PositionX, PositionY}),
	{next_state, playing, State};
playing({pick_object, ClientHandle, Data}, State) ->
	gen_event:notify(State#state.match_hub_pid, {pick_object, ClientHandle, Data}),
	{next_state, playing, State};
playing({timeout, _Ref, end_match_timeout}, State) ->
	do_end_match(State);
playing({end_match, _Data}, State) ->
	do_end_match(State);
playing(_Message, State) ->
	{next_state, playing, State}.

handle_event({join, SessionId, Pid}, StateName, State) ->
	Parameters = [{session_id, SessionId}, {websocket_pid , Pid}],
	% FIXME: link to the connection handler
	ok = gen_event:add_handler(State#state.match_hub_pid, {hs_events_handler, SessionId}, Parameters),
	% FIXME Handle a player leaving.
	hs_match_manager_service:add_player(State#state.match_id, SessionId),
	{next_state, StateName, State};
handle_event({new_player, ClientHandle, PlayerData}, StateName, State) ->
	PlayerStore = State#state.player_store,
	PlayerType = proplists:get_value(<<"type">>, PlayerData, none),
	hs_player_store:add_player(PlayerStore, hs_client_handle:get_session(ClientHandle), PlayerType, PlayerData),
	gen_event:notify(State#state.match_hub_pid, {new_player, ClientHandle, PlayerData}),
	{next_state, StateName, State};
handle_event({player_left, ClientHandle}, StateName, State) ->
	SessionId = hs_client_handle:get_session(ClientHandle),
	PlayerStore = State#state.player_store,
	gen_event:delete_handler(State#state.match_hub_pid, {hs_events_handler, SessionId}, []),
	hs_player_store:remove_player(PlayerStore,SessionId),
	hs_match_manager_service:remove_player(State#state.match_id, SessionId),
	maybe_delete_match (hs_player_store:list_players_by_type(PlayerStore, <<"player">>), StateName, State).

%% Sync events
handle_sync_event({get_objects}, _From, StateName, State) ->
	{ok, DotObjects} = hs_map_store:get_entities_by_type(State#state.map_store, <<"dots">>),
	{ok, MagicDotsObjects} = hs_map_store:get_entities_by_type(State#state.map_store, <<"magicdots">>),
	{reply, DotObjects ++ MagicDotsObjects , StateName, State};
handle_sync_event({list_players},  _From, StateName, State ) ->
	PlayerStore = State#state.player_store,
	PlayerList = hs_player_store:list_players(PlayerStore),
	{reply, PlayerList, StateName, State}.

maybe_delete_match([], _StateName, State) ->
	lager:info("ref: Deleting match ~p",[State#state.match_id]),
	hs_match_manager_service:remove_match(State#state.match_id),
	gen_event:delete_handler(State#state.match_hub_pid, {hs_rules_enforcement, State#state.match_id}, []),
	gen_fsm:cancel_timer(State#state.start_match_timer),
	gen_fsm:cancel_timer(State#state.end_match_timer),
	[hs_ai_ghost:die(AIPid)|| AIPid <- State#state.ai_pids],
	{stop, no_human_players, State};
maybe_delete_match(_HumanPlayers, StateName, State) ->
	{next_state, StateName, State}.