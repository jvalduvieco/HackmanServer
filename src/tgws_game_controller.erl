%% Copyright
-module(tgws_game_controller).
-behaviour(gen_fsm).

%% API
-export([start_link/0]).
-export([login/0, start_game/0, new_player/2, position_update/2]).
-export([init/1, terminate/3]).
-export([waiting/2, waiting/3, playing/2, finished/2]).

-record(state, {next_session = 0}).

start_link() ->
	gen_fsm:start_link({local, game_controller}, tgws_game_controller, [], []).

login() ->
	gen_fsm:sync_send_event(game_controller, {login}).

start_game() ->
	gen_fsm:send_event(game_controller, {start_game}).

new_player(Session, PlayerData) ->
	gen_fsm:send_event(game_controller, {new_player, Session, PlayerData}).

position_update(Session, Message) ->
	gen_fsm:send_event(game_controller, {position_update, Session, Message}).

init([]) ->
	tgws_player_list:init(),
	{ok, waiting, #state{}}.
terminate(_Reason, _StateName, _State) ->
	ok.

%% Sync events
waiting({login}, {Pid, _Tag}, State) ->
	% create a session Id and an event handler
	SessionId = State#state.next_session,
	Parameters = [{session_id, SessionId}, {websocket_pid , Pid}],
	ok = gen_event:add_handler(tgws_game_event_manager, tgws_events_handler, Parameters),
	{reply, {session_id, SessionId} , waiting, State#state{next_session = SessionId + 1}}.

% Async events
waiting({new_player, Session, PlayerData}, State) ->
	tgws_player_list:add_player(Session, PlayerData),
	gen_event:notify(tgws_game_event_manager, {new_player, Session, PlayerData}),
	{next_state, waiting, State};
waiting({start_game}, State) ->
	lager:debug("Starting game..."),
	gen_event:notify(tgws_game_event_manager, {start_game}),
	{next_state, playing, State}.
playing({position_update, From, Message}, State) ->
	gen_event:notify(tgws_game_event_manager, {position_update, From, Message}),
	{next_state, playing, State};
playing({end_game, _Data}, State) ->
	{next_state, finished, State};
playing(_Message, State) ->
	{next_state, playing, State}.

finished({restart, _Data},State) ->
	{next_state, initializing, State};
finished(_Message, State) ->
	{next_state, finished, State}.