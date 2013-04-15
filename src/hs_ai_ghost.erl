%% Copyright
-module(hs_ai_ghost).
-author("jllonch").

%% Test
%% Map = hs_file_map_loader:load("priv/level0.json"). hs_ai_ghost:start_link({1, {32, 64}, Map, {21*32, 23*32}}).
-behaviour(gen_fsm).

%% Config
-define(HS_AI_GHOST_SEARCHING_TIMEOUT, 10000).
-define(HS_AI_GHOST_SPEED, 3).
-define(HS_AI_GHOST_MOVE_TIME, 10).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
%% States
-export([waiting/2, search/2, search/3, pursue/2, pursue/3]).
%% Public API
-export([start_link/1]).
-export([start_game/1, killed/1, end_game/1]).

%% State
-record(state, {
	session,
  map_handle,
  pos = none, % {X, Y}
  velocity_vector = none, % {X, Y}
  target_coord = none, % {X, Y}
  target_id = none,
  map_width,
  map_height,
  timer_ref_status = none,
  timer_ref_movement = none,
	player_data = none,
	player_store = none,
	game_event_manager_pid = none
}).

%% API
start_link({Id, {X, Y}, Map, {MapWidth, MapHeight}, PlayerData, PlayerStore, GameEventManagerPid}) ->
	gen_fsm:start_link(?MODULE, {Id, {X, Y}, Map, {MapWidth, MapHeight}, PlayerData, PlayerStore, GameEventManagerPid}, []).

%% Public API
start_game(GhostAIPid) ->
	gen_fsm:send_event(GhostAIPid, {start_game}).
end_game(GhostAIPid) ->
	gen_fsm:send_event(GhostAIPid, {end_game}).
killed(GhostAIPid) ->
	gen_fsm:send_event(GhostAIPid, {killed}).

%% Callbacks
init({Session, {X, Y}, Map, {MapWidth, MapHeight}, PlayerData, PlayerStore, GameEventManagerPid}) ->
  lager:debug("initializing ghost ai..."),
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A,B,C}),
	hs_player_store:add_player(PlayerStore, Session, <<"ghostAI">>, PlayerData),
	ok = gen_event:add_handler(hs_game_event_manager, hs_ai_ghost_events_handler, erlang:self()),
  {ok, waiting,
	  #state{session = Session, pos={X, Y}, map_handle = Map, map_width = MapWidth, map_height = MapHeight,
  player_data = PlayerData, player_store = PlayerStore, game_event_manager_pid = GameEventManagerPid}}.

%% gen_fsm callbacks
%% ---------------------------------------------------
%% State: waiting
%% ---------------------------------------------------
waiting({start_game}, State) ->
	TargetCoords = choose_target_coords(State#state.map_width, State#state.map_height),
	% status timer
	StatusTimer = create_status_timer(),
	% move timer
	MoveTimer = create_move_timer(),
	% make decision
	Velocity = take_decision(State#state.map_handle, State#state.pos, State#state.velocity_vector, TargetCoords),
	{next_state, search,
		State#state{timer_ref_status = StatusTimer, timer_ref_movement = MoveTimer, velocity_vector = Velocity, target_coord = TargetCoords}}.
%% ---------------------------------------------------
%% State: search
%% ---------------------------------------------------
search({timeout, _Ref, move_timeout}, State) ->
	% Just update our current position and schedule a new timer
	% TODO Check for collisions
  NewPos = move(State#state.pos, State#state.velocity_vector),
	%lager:debug("params ~p ~p ~p ~p",[State#state.map_handle, State#state.pos, State#state.velocity_vector, State#state.target_coord]),
	NewVel = take_decision(State#state.map_handle, State#state.pos, State#state.velocity_vector, State#state.target_coord),
	maybe_announce(State#state.velocity_vector, NewVel, NewPos, State),
  MoveTimerRef = create_move_timer(),
  {next_state, search, State#state{pos = NewPos, timer_ref_movement = MoveTimerRef, velocity_vector = NewVel}};
search({timeout, _Ref, status_timeout}, State) ->
  TargetPacman = choose_target_pacman(State#state.player_store),
	TargetCoords = hs_player_store:get_player_position(State#state.player_store, TargetPacman),
	%lager:debug("params ~p ~p ~p ~p",[State#state.map_handle, State#state.pos, State#state.velocity_vector, TargetCoords]),
	Velocity = take_decision(State#state.map_handle, State#state.pos, State#state.velocity_vector, TargetCoords),
	StatusTimer = create_status_timer(),
  {next_state, pursue,
	  State#state{target_id = TargetPacman, target_coord = TargetCoords, velocity_vector = Velocity,
	  timer_ref_status = StatusTimer}}.

search(_Event, _From, State) ->
  {reply, ok, search, State}.

%% ---------------------------------------------------
%% State: pursue
%% ---------------------------------------------------
pursue({timeout, _Ref, move_timeout}, State) ->
  %lager:debug("[pursue] move_timeout"),
  TargetCoords = recalculate_target_pacman_coord(State#state.player_store, State#state.target_id),
	NewVel = take_decision(State#state.map_handle, State#state.pos, State#state.velocity_vector, TargetCoords),
  NewPos = move(State#state.pos, NewVel),
	maybe_announce(State#state.velocity_vector, NewVel, NewPos, State),
  MoveTimerRef = create_move_timer(),
  {next_state, pursue, State#state{velocity_vector = NewVel, pos = NewPos, timer_ref_movement = MoveTimerRef}};
pursue({timeout, _Ref, status_timeout}, State) ->
  TargetCoords = choose_target_coords(State#state.map_width, #state.map_height),
	Velocity = take_decision(State#state.map_handle, State#state.pos, State#state.velocity_vector, TargetCoords),
  StatusTimer = create_status_timer(),
  {next_state, search,
	  State#state{ target_id=none, target_coord = TargetCoords, velocity_vector = Velocity, timer_ref_status = StatusTimer}}.

pursue(_Event, _From, State) ->
  {reply, ok, pursue, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% private functions
maybe_announce(OldVelocity, CurrentVelocity, Position, State) when OldVelocity =:= CurrentVelocity ->
	ClientHandle=hs_client_handle:init(State#state.session, self()),
	{VelX, VelY} = CurrentVelocity,
	{PosX, PosY} = Position,
	Velocity = {<<"vel">>, [{<<"x">>, VelX},{<<"y">>, VelY}]},
	PositionFormatted = {<<"pos">>, [{<<"x">>, PosX},{<<"y">>, PosY}]},
	lager:debug("Announcing ~p (~p)",[PositionFormatted, Velocity]),
	gen_event:notify(State#state.game_event_manager_pid,
		{position_update, ClientHandle, State#state.player_data ++ [Velocity , PositionFormatted]});
maybe_announce(_OldVelocity, _CurrentVelocity, _Position, _State) ->
	ok.
create_move_timer() ->
  gen_fsm:start_timer(?HS_AI_GHOST_MOVE_TIME, move_timeout).

create_status_timer() ->
  gen_fsm:start_timer(?HS_AI_GHOST_SEARCHING_TIMEOUT, status_timeout).

move(CurrentPosition, Velocity) ->
  {VelocityVectorX, VelocityVectorY} = Velocity,
  {OriginalPosX, OriginalPosY} = CurrentPosition,
	{OriginalPosX+VelocityVectorX, OriginalPosY+VelocityVectorY}.

choose_target_coords(MapWidth, MapHeight) ->
  % choose a quadrant
  case random:uniform(4) of
    1 -> {MapWidth * 32, 0};
    2 -> {MapWidth * 32, MapHeight * 32};
    3 -> {0, MapWidth * 32};
    4 -> {0, 0}
  end.

choose_target_pacman(PlayerStore) ->
  PacmanInGame = hs_player_store:list_players_by_type(PlayerStore, <<"pacman">>),
	TargetPacman = lists:nth(random:uniform(length(PacmanInGame)), PacmanInGame),
	proplists:get_value(<<"sessionId">>, TargetPacman, none).

recalculate_target_pacman_coord(PlayerStore, TargetID) ->
	hs_player_store:get_player_position(PlayerStore, TargetID).

take_decision(MapHandle, CurrentPosition, CurrentVelocity, TargetCoordinates) ->
	TilePos = translate_coord_to_tile(CurrentPosition),
  {ok, ListTilePositions} = hs_map_store:free_move_positions(MapHandle, TilePos),
	TargetTile = translate_coord_to_tile(TargetCoordinates),
  choose_velocity_vector(TilePos, ListTilePositions, TargetTile, CurrentVelocity).

choose_velocity_vector(_, [CoordPosition|[]], _, _) ->
  % one option only
  CoordPosition;
choose_velocity_vector(TilePos, ListTilePositions, TileTarget, CurrentVelocityVector) ->
  ListTilePositionsDistance = [{distance(CoordPos, TileTarget), calculate_velocity_vector(CoordPos, TilePos)} || CoordPos <- ListTilePositions],
  ListTilePositionsDistanceSort = lists:sort(fun({DistA, _}, {DistB, _}) -> if(DistA=<DistB) -> true; true -> false end end, ListTilePositionsDistance),
  % choose one option
  velocity_vector_choose_from_list(ListTilePositionsDistanceSort, reverse_vector(CurrentVelocityVector)).

%% select one vector that is nearest but try to avoid to go back
velocity_vector_choose_from_list([{_, VelocityVector} | []], _) ->
  VelocityVector;
velocity_vector_choose_from_list([{_, VelocityVector} | TailListCoordPositionsDistanceSort], VelocityVector) ->
  velocity_vector_choose_from_list(TailListCoordPositionsDistanceSort, VelocityVector);
velocity_vector_choose_from_list([{_, VelocityVector} | _], _) ->
  VelocityVector.

distance({PosX, PosY}, {TargetCoordX, TargetCoordY}) ->
  math:sqrt(math:pow(TargetCoordX - PosX, 2) + math:pow(TargetCoordY - PosY, 2)).

calculate_velocity_vector({CurrentCoordX, Same}, {CoordPosX, Same}) when CoordPosX >= CurrentCoordX -> {-?HS_AI_GHOST_SPEED,  0};
calculate_velocity_vector({CurrentCoordX, Same}, {CoordPosX, Same}) when CoordPosX < CurrentCoordX -> { ?HS_AI_GHOST_SPEED,  0};
calculate_velocity_vector({Same, CurrentCoordY}, {Same, CoordPosY}) when CoordPosY >= CurrentCoordY -> { 0, -?HS_AI_GHOST_SPEED};
calculate_velocity_vector({Same, CurrentCoordY}, {Same, CoordPosY}) when CoordPosY < CurrentCoordY -> { 0,  ?HS_AI_GHOST_SPEED}.

translate_coord_to_tile({X, Y}) ->
  TileX = erlang:round(X / 32),
  TileY = erlang:round(Y / 32),
  {TileX, TileY}.

reverse_vector(none) ->
  none;
reverse_vector({X, Y}) ->
  {X * -1, Y * -1}.