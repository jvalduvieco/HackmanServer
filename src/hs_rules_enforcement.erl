%% Copyright
-module(hs_rules_enforcement).
-author("jvalduvieco").

-behaviour(gen_event).

%% gen_event
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
	code_change/3]).

%%FIXME Get from map handle
-define(HS_TILE_HEIGHT, 32).
-define(HS_TILE_WIDTH, 32).

%% gen_event callbacks
-record(state, {map, player_store}).

init({PlayerStore}) ->
	lager:info("Rules enforcement registered ~p ", [PlayerStore]),
	{ok, Map} = hs_file_map_loader:load(hs_config:get(map_file)),
	{ok, #state{map = Map, player_store = PlayerStore}}.

handle_event({pick_object, ClientHandle, Data}, State) ->
	ObjectId = proplists:get_value(<<"objectId">>, Data),
	ObjectType = proplists:get_value(<<"objectType">>, Data),
	maybe_give_points(ObjectType, hs_map_store:delete_entity_check_type(State#state.map, ObjectId, ObjectType), ClientHandle),
	{ok, State};
handle_event({ghost_ai_position_update, GhostAiPid, TilePos}, State) ->
  lager:debug("hs_rules_enforcement: ai_ghost_position_update"),
  ghost_ai_check_killed(GhostAiPid, TilePos, State#state.player_store),
  {ok, State};

handle_event(_Event, State) ->
%% 	lager:debug("hs_rules_enforcement: handle_event ~p ..",[Event]),
	{ok, State}.

handle_call(_Request, State) ->
	lager:debug("hs_rules_enforcement: handle_call.."),
	{ok, reply, State}.

handle_info(_Info, State) ->
	lager:debug("hs_rules_enforcement: handle_info.."),
	{ok, State}.

terminate(_Arg, _State) ->
	lager:debug("hs_rules_enforcement: terminate.."),
	ok.

code_change(_OldVsn, State, _Extra) ->
	lager:debug("hs_rules_enforcement: code_change.."),
	{ok, State}.


maybe_give_points(_ObjectType, NumberOfObjects, Player) when NumberOfObjects =:= 0 ->
	lager:info("A player picked an already picked object. {player:~p, Objects: ~p", [Player, NumberOfObjects]);
maybe_give_points(<<"dots">>, _NumberOfObjects, ClientHandle) ->
	hs_client_handle:get_gateway_pid(ClientHandle) ! {reply, [{<<"type">>, <<"updatePointsAnnounce">>}, {<<"data">>, [{<<"amount">>, 100}]}]};
maybe_give_points(<<"magicdots">>, _NumberOfObjects, ClientHandle) ->
	hs_client_handle:get_gateway_pid(ClientHandle) ! {reply, [{<<"type">>, <<"updatePointsAnnounce">>},
		{<<"data">>,[{<<"amount">>, 500}]}]}.


ghost_ai_check_killed(GhostAiPid, TilePos, PlayerStore) ->
  PlayersList = hs_player_store:list_players_by_coords(PlayerStore, TilePos, {?HS_TILE_WIDTH, ?HS_TILE_HEIGHT}),
%%   lager:debug("[ghost_ai_check_killed] PlayersList: ~p", [PlayersList]),
  ghost_ai_check_killed_players_on_me(GhostAiPid, PlayersList).
ghost_ai_check_killed_players_on_me(_, []) ->
  false;
ghost_ai_check_killed_players_on_me(GhostAiPid, PlayersList) ->
  %[PlayerInfo | _] = PlayersList,
  %{Session, PlayerCoord, PlayerCoordRed, PlayerData} = PlayerInfo,
  hs_ai_ghost:killed(GhostAiPid),
  true.
