%% Copyright
-module(hs_game_controller).
-behaviour(gen_fsm).

%% API
-export([start_link/0]).
-export([login/0, start_game/0, new_player/2, get_objects/0, position_update/2, pick_object/2, list_players/0]).
-export([init/1, code_change/4, handle_info/3, handle_sync_event/4, handle_event/3, terminate/3]).
-export([waiting/2, waiting/3, playing/2, finished/2]).

-record(state, {next_session = 0, map, player_store}).

%%TODO: Refactor to a gen_server
start_link() ->
	gen_fsm:start_link({local, game_controller}, hs_game_controller, [], []).

login() ->
	gen_fsm:sync_send_event(game_controller, {login}).

start_game() ->
	gen_fsm:send_event(game_controller, {start_game}).

new_player(Session, PlayerData) ->
	gen_fsm:send_event(game_controller, {new_player, Session, PlayerData}).

list_players() ->
	gen_fsm:sync_send_event(game_controller, {list_players}).

get_objects() ->
	gen_fsm:sync_send_event(game_controller, {get_objects}).

position_update(Session, Message) ->
	gen_fsm:send_event(game_controller, {position_update, Session, Message}).

pick_object(Session, Message) ->
	gen_fsm:send_event(game_controller, {pick_object, Session, Message}).

init([]) ->
	Map = hs_file_map_loader:load(hs_config:get(map_file)),
	{ok, PlayerStore} = hs_player_store:init(),
	{ok, GameEventManagerPid} = gen_event:start_link({local, hs_game_event_manager}),
	% FIXME: Add a supervised event handler
	ok = gen_event:add_handler(hs_game_event_manager, hs_rules_enforcement, []),
	% Add IA's
	Session = 0,
	PlayerId = <<"GhostAI0">>,
	PlayerData = [{<<"sessionId">>, Session},{<<"playerId">>, PlayerId}, {<<"type">>, <<"ghostAI">>}] ,
	{ok, _AiPid} = hs_ai_ghost:start_link({Session, {32, 96}, Map, {20, 22}, PlayerData, PlayerStore, GameEventManagerPid}),
	{ok, waiting, #state{map=Map, player_store=PlayerStore, next_session = Session + 1}}.
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
waiting({login}, {Pid, _Tag}, State) ->
	% create a session Id and an event handler
	SessionId = State#state.next_session,
	Parameters = [{session_id, SessionId}, {websocket_pid , Pid}],
	% FIXME: link to the connection handler
	ok = gen_event:add_handler(hs_game_event_manager, hs_events_handler, Parameters),
	{reply, {session_id, SessionId} , waiting, State#state{next_session = SessionId + 1}};
waiting({get_objects}, {_Pid, _Tag}, State) ->
	{ok, DotObjects} = hs_map_store:get_entities_by_type(State#state.map, <<"dots">>),
	{ok, MagicDotsObjects} = hs_map_store:get_entities_by_type(State#state.map, <<"magicdots">>),
	{reply, DotObjects ++ MagicDotsObjects , waiting, State};
waiting({list_players}, {_Pid, _Tag}, State ) ->
	PlayerStore = State#state.player_store,
	PlayerList = hs_player_store:list_players(PlayerStore),
	{reply, PlayerList,waiting, State}.

% Async events
waiting({new_player, ClientHandle, PlayerData}, State) ->
	PlayerStore = State#state.player_store,
	hs_player_store:add_player(PlayerStore, hs_client_handle:get_session(ClientHandle), PlayerData),
	gen_event:notify(hs_game_event_manager, {new_player, ClientHandle, PlayerData}),
	{next_state, waiting, State};
waiting({start_game}, State) ->
	lager:debug("Starting game..."),
	gen_event:notify(hs_game_event_manager, {start_game}),
	{next_state, playing, State}.

playing({position_update, ClientHandle, PlayerData}, State) ->
	PlayerStore = State#state.player_store,
	Position = proplists:get_value(<<"pos">>, PlayerData),
	PositionX = proplists:get_value(<<"x">>, Position),
	PositionY = proplists:get_value(<<"y">>, Position),
	hs_player_store:update_player_position(PlayerStore, hs_client_handle:get_session(ClientHandle), {PositionX, PositionY}),
	gen_event:notify(hs_game_event_manager, {position_update, ClientHandle, PlayerData}),
	{next_state, playing, State};
playing({pick_object, ClientHandle, Data}, State) ->
	gen_event:notify(hs_game_event_manager, {pick_object, ClientHandle, Data}),
	{next_state, playing, State};
playing({end_game, _Data}, State) ->
	{next_state, finished, State};
playing(_Message, State) ->
	{next_state, playing, State}.

finished({restart, _Data},State) ->
	{next_state, initializing, State};
finished(_Message, State) ->
	{next_state, finished, State}.