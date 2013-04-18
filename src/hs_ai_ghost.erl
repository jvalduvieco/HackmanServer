%% Copyright
-module(hs_ai_ghost).
-author("jllonch").

%% Test
%% Map = hs_file_map_loader:load("priv/level0.json"). hs_ai_ghost:start_link({1, {32, 64}, Map, {21*32, 23*32}}).
-behaviour(gen_fsm).

%% Config
-define(HS_AI_GHOST_STATUS_TIMEOUT, 10000).
-define(HS_AI_GHOST_SPEED, 3).
-define(HS_AI_GHOST_MOVE_TIME, 16).
%%FIXME Get from map handle
-define(HS_TILE_HEIGHT, 32).
-define(HS_TILE_WIDTH, 32).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
%% States
-export([waiting/2, search/2, search/3, pursue/2, pursue/3]).
%% Public API
-export([start_link/1]).
-export([start_match/1, killed/1, end_match/1]).

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
start_match(GhostAIPid) ->
	gen_fsm:send_event(GhostAIPid, {start_match}).
end_match(GhostAIPid) ->
	gen_fsm:send_event(GhostAIPid, {end_match}).
killed(GhostAIPid) ->
	gen_fsm:send_all_state_event(GhostAIPid, {killed}).

%% Callbacks
init({Session, {X, Y}, Map, {MapWidth, MapHeight}, PlayerData, PlayerStore, GameEventManagerPid}) ->
  lager:debug("initializing ghost ai..."),
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A,B,C}),
	hs_player_store:add_player(PlayerStore, Session, <<"ghostAI">>, PlayerData),
	ok = gen_event:add_handler(GameEventManagerPid, hs_ai_ghost_events_handler, erlang:self()),
  {ok, waiting,
	  #state{session = Session, pos={X, Y}, map_handle = Map, map_width = MapWidth, map_height = MapHeight,
  player_data = PlayerData, player_store = PlayerStore, game_event_manager_pid = GameEventManagerPid}}.

%% gen_fsm callbacks
%% ---------------------------------------------------
%% State: waiting
%% ---------------------------------------------------
waiting({start_match}, State) ->
  lager:debug("[waiting]"),
%%   lager:debug("pos: ~p; map_width: ~p; map_height: ~p", [State#state.pos, State#state.map_width, State#state.map_height]),
	TargetCoords = choose_target_coords(State#state.pos, State#state.map_width, State#state.map_height),
%%   lager:debug("TargetCoords: ~p", [TargetCoords]),
  % make decision
  Velocity = take_decision(State#state.map_handle, State#state.pos, State#state.velocity_vector, TargetCoords),
	% status timer
	StatusTimer = create_status_timer(),
	% move timer
	MoveTimer = create_move_timer(),
  {next_state, search,
		State#state{timer_ref_status = StatusTimer, timer_ref_movement = MoveTimer, velocity_vector = Velocity, target_coord = TargetCoords}}.


%% ---------------------------------------------------
%% State: search
%% ---------------------------------------------------
search({timeout, _Ref, move_timeout}, State) ->
	% Just update our current position and schedule a new timer
	%lager:debug("params ~p ~p ~p ~p",[State#state.map_handle, State#state.pos, State#state.velocity_vector, State#state.target_coord]),
	NewTargetCoord = maybe_change_target_coord(State#state.pos, State#state.target_coord, State#state.map_width, State#state.map_height),
  NewVel = take_decision(State#state.map_handle, State#state.pos, State#state.velocity_vector, NewTargetCoord),
  NewPos = move(State#state.pos, NewVel),
	maybe_announce_velocity(State#state.velocity_vector, NewVel, NewPos, State),
	maybe_announce_position(State#state.pos, NewPos, State#state.game_event_manager_pid),
  MoveTimerRef = create_move_timer(),
  {next_state, search, State#state{pos = NewPos, timer_ref_movement = MoveTimerRef, velocity_vector = NewVel, target_coord = NewTargetCoord}};
search({timeout, _Ref, status_timeout}, State) ->
  lager:debug("[search]"),
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
	maybe_announce_velocity(State#state.velocity_vector, NewVel, NewPos, State),
  maybe_announce_position(State#state.pos, NewPos, State#state.game_event_manager_pid),
  MoveTimerRef = create_move_timer(),
  {next_state, pursue, State#state{velocity_vector = NewVel, pos = NewPos, timer_ref_movement = MoveTimerRef}};
pursue({timeout, _Ref, status_timeout}, State) ->
  lager:debug("[pursue]"),
  TargetCoords = choose_target_coords(State#state.pos, State#state.map_width, #state.map_height),
	Velocity = take_decision(State#state.map_handle, State#state.pos, State#state.velocity_vector, TargetCoords),
  StatusTimer = create_status_timer(),
  {next_state, search,
	  State#state{ target_id=none, target_coord = TargetCoords, velocity_vector = Velocity, timer_ref_status = StatusTimer}}.

pursue(_Event, _From, State) ->
  {reply, ok, pursue, State}.

handle_event({killed}, _StateName, State) ->
  lager:debug("killed"),
  NewState = State#state{pos={11 * ?HS_TILE_WIDTH, 12 * ?HS_TILE_HEIGHT}}, % TODO: ghost house
  {next_state, search, NewState};
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
maybe_announce_velocity(OldVelocity, CurrentVelocity, Position, State) when OldVelocity =/= CurrentVelocity ->
	ClientHandle=hs_client_handle:init(State#state.session, self()),
	{VelX, VelY} = CurrentVelocity,
	{PosX, PosY} = normalize_coords(Position),
	Velocity = {<<"vel">>, [{<<"x">>, VelX},{<<"y">>, VelY}]},
	PositionFormatted = {<<"pos">>, [{<<"x">>, PosX}, {<<"y">>, PosY}]},
	%lager:debug("Announcing ~p (~p , ~p)",[PositionFormatted, OldVelocity, CurrentVelocity]),
	gen_event:notify(State#state.game_event_manager_pid,
		{position_update, ClientHandle, State#state.player_data ++ [Velocity, PositionFormatted]});
maybe_announce_velocity(_OldVelocity, _CurrentVelocity, _Position, _State) ->
	ok.

maybe_announce_position(OldPos, NewPos, GameEventManagerPid) ->
  maybe_announce_tiled_position(translate_coord_to_tile(OldPos), translate_coord_to_tile(NewPos), GameEventManagerPid).
maybe_announce_tiled_position(OldTilePos, NewTilePos, GameEventManagerPid) when OldTilePos =/= NewTilePos ->
  gen_event:notify(GameEventManagerPid, {ghost_ai_position_update, self(), NewTilePos});
maybe_announce_tiled_position(_OldTilePos, _NewTilePos, _GameEventManagerPid) ->
  ok.

create_move_timer() ->
  gen_fsm:start_timer(?HS_AI_GHOST_MOVE_TIME, move_timeout).

create_status_timer() ->
  gen_fsm:start_timer(?HS_AI_GHOST_STATUS_TIMEOUT, status_timeout).

move(CurrentPosition, Velocity) ->
  {VelocityVectorX, VelocityVectorY} = Velocity,
  {OriginalPosX, OriginalPosY} = CurrentPosition,
	{OriginalPosX+VelocityVectorX, OriginalPosY+VelocityVectorY}.

%% change target coordinates if current position is in the target coordinates
maybe_change_target_coord(CurrentPosition, TargetCoordinates, MapWidth, MapHeight) ->
  {TilePosX, TilePosY} = translate_coord_to_tile(CurrentPosition),
  {TileTargetX, TileTargetY} = translate_coord_to_tile(TargetCoordinates),
  Distance = math:sqrt(math:pow(TilePosX - TileTargetX, 2) + math:pow(TilePosY - TileTargetY, 2)),
%%   lager:debug("Distance: ~p", [Distance]),
  maybe_change_target_coord(Distance, MapWidth, MapHeight, CurrentPosition, TargetCoordinates).
maybe_change_target_coord(Distance, MapWidth, MapHeight, CurrentPosition, _) when Distance < 3 ->
%%   lager:debug("target coord reached"),
%%   lager:debug("  CurrentPosition: ~p", [CurrentPosition]),
%%   lager:debug("  MapWidth: ~p", [MapWidth]),
%%   lager:debug("  MapHeight: ~p", [MapHeight]),
  choose_target_coords(CurrentPosition, MapWidth, MapHeight);
maybe_change_target_coord(_, _, _, _, TargetCoordinates) ->
  TargetCoordinates.

%% choose a target quadrant coords that it is diffent of your current quadrant
choose_target_coords({X, Y}, MapWidth, MapHeight) when (X >= MapWidth/2) and (Y < MapHeight/2) -> % 1st quad
  AvailableQuads = [2, 3, 4],
  choose_target_coords_available_quads(AvailableQuads, MapWidth, MapHeight);
choose_target_coords({X, Y}, MapWidth, MapHeight) when (X >= MapWidth/2) and (Y >= MapHeight/2) -> % 2nd quad
  AvailableQuads = [1, 3, 4],
  choose_target_coords_available_quads(AvailableQuads, MapWidth, MapHeight);
choose_target_coords({X, Y}, MapWidth, MapHeight) when (X < MapWidth/2) and (Y >= MapHeight/2) -> % 3th quad
  AvailableQuads = [1, 2, 4],
  choose_target_coords_available_quads(AvailableQuads, MapWidth, MapHeight);
choose_target_coords({X, Y}, MapWidth, MapHeight) when (X < MapWidth/2) and (Y < MapHeight/2) -> % 4th quad
  AvailableQuads = [1, 2, 3],
  choose_target_coords_available_quads(AvailableQuads, MapWidth, MapHeight).
choose_target_coords_available_quads(AvailableQuads, MapWidth, MapHeight) ->
  ChooseQuad = lists:nth(random:uniform(3), AvailableQuads),
  Quads = [ {MapWidth * ?HS_TILE_WIDTH, 0}, % 1st quad
            {MapWidth * ?HS_TILE_WIDTH, MapHeight * ?HS_TILE_HEIGHT}, % 2nd quad
            {0, MapHeight * ?HS_TILE_HEIGHT}, % 3th quad
            {0, 0}], % 4th quad
%%   lager:debug("AvailableQuads: ~p; ChooseQuad: ~p", [AvailableQuads, ChooseQuad]),
  lists:nth(ChooseQuad, Quads).

choose_target_pacman(PlayerStore) ->
	% FIXME Handle when no pacmans are in match
  PacmanInGame = hs_player_store:list_players_by_type(PlayerStore, <<"player">>),
	lager:debug("Pacmans in game: ~p", [PacmanInGame]),
	lists:nth(random:uniform(length(PacmanInGame)), PacmanInGame).

recalculate_target_pacman_coord(PlayerStore, TargetID) ->
	hs_player_store:get_player_position(PlayerStore, TargetID).

take_decision(MapHandle, CurrentPosition, CurrentVelocity, TargetCoordinates) ->
	TilePos = translate_coord_to_tile(CurrentPosition),
  {ok, ListTilePositions} = hs_map_store:free_move_positions(MapHandle, TilePos),
	TargetTile = translate_coord_to_tile(TargetCoordinates),
  Result = choose_velocity_vector(TilePos, ListTilePositions, TargetTile, CurrentVelocity),
%%   lager:debug("  TilePos: ~p (~p);  TargetTile: ~p;  Velocity: ~p;  ListTilePositions: ~p", [TilePos, CurrentPosition, TargetTile, Result, ListTilePositions]),
  Result.

choose_velocity_vector(TilePos, [CoordPosition|[]], _, _) ->
  % one option only
  calculate_velocity_vector(CoordPosition, TilePos);
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
  TileX = erlang:round(X / ?HS_TILE_WIDTH),
  TileY = erlang:round(Y / ?HS_TILE_HEIGHT),
  {TileX, TileY}.
%% Only allow coordinates multiple of HS_TILE_HEIGHT
normalize_coords(Coords) ->
	{X,Y} = translate_coord_to_tile(Coords),
	{X * ?HS_TILE_WIDTH, Y * ?HS_TILE_HEIGHT}.
reverse_vector(none) ->
  none;
reverse_vector({X, Y}) ->
  {X * -1, Y * -1}.






