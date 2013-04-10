%% Copyright
-module(hs_ai_ghost).
-author("jllonch").

%% Test
%% Map = hs_file_map_loader:load("priv/level0.json").
%% hs_ai_ghost:start_link({1, {16, 16}, Map, {21*32, 23*32}}).

-behaviour(gen_fsm).

%% Config
-define(HS_AI_GHOST_SEARCHING_TIMEOUT, 10000).
-define(HS_AI_GHOST_SPEED, 10).
-define(HS_AI_GHOST_MOVE_TIME, 100).

%% API
-export([start_link/1]).

%% gen_fsm
-export([init/1, search/2, search/3, pursue/2, pursue/3, handle_event/3,
  handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% API
start_link({Id, {X, Y}, Map, {MapWidth, MapHeight}}) ->
  gen_fsm:start_link(?MODULE, {Id, {X, Y}, Map, {MapWidth, MapHeight}}, []).

%% gen_fsm callbacks
-record(state, {
  id,
  map_handle,
  pos=none, % {X, Y}
  velocity_vector=none, % {X, Y}
  target_coord=none, % {X, Y}
  map_width,
  map_height,
  timer_ref_status=none,
  timer_ref_movement=none
}).

init({Id, {X, Y}, Map, {MapWidth, MapHeight}}) ->
  lager:debug("initializing ai ghost..."),
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A,B,C}),
  State = #state{id=Id, pos={X, Y}, map_handle=Map, map_width=MapWidth, map_height=MapHeight},
  % make decision
  NewState = make_decision(State),
  % move timer
  NewStateMove = create_move_timer(NewState),
  {ok, search, NewStateMove}.

%% ---------------------------------------------------
%% State: search
%% ---------------------------------------------------
search({timeout, _Ref, move_timeout}, State) ->
  lager:debug("[search] move_timeout"),
  StateMove = move(State),
  NewState = create_move_timer(StateMove),
  {next_state, search, NewState};
search({timeout, _Ref, status_timeout}, State) ->
  lager:debug("[search] status_timeout"),
  NewState = make_decision(State#state{target_coord=none}),
  {next_state, pursue, NewState}.

search(_Event, _From, State) ->
  {reply, ok, search, State}.

%% ---------------------------------------------------
%% State: pursue
%% ---------------------------------------------------
pursue({timeout, _Ref, move_timeout}, State) ->
  lager:debug("[pursue] move_timeout"),
  StateMove = move(State),
  NewState = create_move_timer(StateMove),
  {next_state, pursue, NewState};
pursue({timeout, _Ref, status_timeout}, State) ->
  lager:debug("[pursue] status_timeout"),
  NewState = make_decision(State#state{target_coord=none}),
  {next_state, search, NewState}.

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
create_move_timer(State) when State#state.timer_ref_movement=/=none ->
  create_move_timer(State#state{timer_ref_movement=none});
create_move_timer(State) ->
  TimerRef = gen_fsm:start_timer(?HS_AI_GHOST_MOVE_TIME, move_timeout),
  State#state{timer_ref_movement=TimerRef}.

move(State) ->
  {VelocityVectorX, VelocityVectorY} = State#state.velocity_vector,
  {PosX, PosY} = State#state.pos,
  NewPos = {PosX+VelocityVectorX, PosY+VelocityVectorY},
  lager:debug("move: ~p", [NewPos]),
  StateMove = State#state{pos=NewPos},
  make_decision(StateMove).

make_decision(State) when State#state.target_coord==none ->
  % choose a quadrant
  TargetCoord = case random:uniform(4) of
    1 -> {State#state.map_width, 0};
    2 -> {State#state.map_width, State#state.map_height};
    3 -> {0, State#state.map_height};
    4 -> {0, 0}
  end,
  NewState = State#state{target_coord=TargetCoord},
  make_decision(NewState);
make_decision(State) when State#state.timer_ref_status=/=none ->
  % destroy timer
  gen_fsm:cancel_timer(State#state.timer_ref_status),
  NewState = State#state{timer_ref_status=none},
  make_decision(NewState);
make_decision(State) ->
  lager:debug("make_decision"),
  % set speed vector
  VelocityVector = velocity_vector(State#state.map_handle, State#state.pos, State#state.target_coord, State#state.velocity_vector),
  lager:debug("make_decision: ~p", [VelocityVector]),
  % create searching timer
  TimerSearchingRef = gen_fsm:start_timer(?HS_AI_GHOST_SEARCHING_TIMEOUT, status_timeout),
  % return new state
  State#state{timer_ref_status=TimerSearchingRef, velocity_vector=VelocityVector}.

velocity_vector(MapHandle, Pos, TargetCoord, CurrentVelocityVector) ->
  lager:debug("  Pos: ~p", [Pos]),
  lager:debug("  TargetCoord: ~p", [TargetCoord]),
  lager:debug("  CurrentVelocityVector: ~p", [CurrentVelocityVector]),
  {X, Y} = translate_coord_to_tile(Pos),
  {ok, ListTilePositions} = hs_map_store:free_move_positions(MapHandle, {X, Y}),
  lager:debug("  ListTilePositions: ~p", [ListTilePositions]),
  TilePos = translate_coord_to_tile(Pos),
  TileTarget = translate_coord_to_tile(TargetCoord),
  lager:debug("  TilePos: ~p", [TilePos]),
  lager:debug("  TileTarget: ~p", [TileTarget]),
  velocity_vector_choose(TilePos, ListTilePositions, TileTarget, CurrentVelocityVector).

velocity_vector_choose(_, [CoordPosition|[]], _, _) ->
  % one option only
  CoordPosition;
velocity_vector_choose(TilePos, ListTilePositions, TileTarget, CurrentVelocityVector) ->
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
  math:sqrt(math:pow(TargetCoordX-PosX, 2) + math:pow(TargetCoordY-PosY, 2)).

calculate_velocity_vector({CoordPosX, Same}, {CurrentCoordX, Same}) when CoordPosX>=CurrentCoordX -> {-?HS_AI_GHOST_SPEED,  0};
calculate_velocity_vector({CoordPosX, Same}, {CurrentCoordX, Same}) when CoordPosX< CurrentCoordX -> { ?HS_AI_GHOST_SPEED,  0};
calculate_velocity_vector({Same, CoordPosY}, {Same, CurrentCoordY}) when CoordPosY>=CurrentCoordY -> { 0, -?HS_AI_GHOST_SPEED};
calculate_velocity_vector({Same, CoordPosY}, {Same, CurrentCoordY}) when CoordPosY< CurrentCoordY -> { 0,  ?HS_AI_GHOST_SPEED}.

translate_coord_to_tile({X, Y}) ->
  TileX = erlang:round(X/32),
  TileY = erlang:round(Y/32),
  {TileX, TileY}.

translate_tile_to_coord({TileX, TileY}) ->
  X = TileX*32,
  Y = TileY*32,
  {X, Y}.

reverse_vector(none) ->
  none;
reverse_vector({X, Y}) ->
  {X*-1, Y*-1}.