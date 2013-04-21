-module(hs_player_store).


%% API
-export([init/0, add_player/4, remove_player/2, list_players/1, list_players_by_type/2, list_players_by_coords/3, update_player_position/3, get_player_position/2]).

%% TODO: Refactor this into a service
%% TODO: Make ETS private
init() ->
% Create a new ETS table, called from supervisor
	Id = make_ref(),
	TableName = namespace(Id),
	ets:new(TableName,[set, public, named_table, {write_concurrency, true},  {read_concurrency, true}]),
	{ok, {TableName}}.
add_player(PlayerStoreHandle, Session, PlayerType, PlayerData) ->
	TableName = get_table_name(PlayerStoreHandle),
	ets:insert(TableName,{Session,{none, none}, PlayerType, PlayerData}).
remove_player(PlayerStoreHandle, SessionId) ->
	TableName = get_table_name(PlayerStoreHandle),
	ets:delete(TableName, SessionId).
update_player_position(PlayerStoreHandle, Key, Position) ->
	TableName = get_table_name(PlayerStoreHandle),
	ets:update_element(TableName, Key, {2, Position}).
get_player_position(PlayerStoreHandle, Key) ->
	TableName = get_table_name(PlayerStoreHandle),
	% FIXME replace by a lookup?
	ets:lookup_element(TableName, Key, 2).
list_players(PlayerStoreHandle) ->
	TableName = get_table_name(PlayerStoreHandle),
	ets:select(TableName, [{{'_', '_', '_', '$1'}, [], ['$1']}]).
list_players_by_type(PlayerStoreHandle, PlayerType) ->
	% FIXME Maybe, create an index table to search by PlayerType
	TableName = get_table_name(PlayerStoreHandle),
	ets:select(TableName, [{{'$1', '$2', PlayerType, '$3'}, [], ['$1']}]).
list_players_by_coords(PlayerStoreHandle, Coord, Reduction) ->
  TableName = get_table_name(PlayerStoreHandle),
  Players = ets:select(TableName, [{{'$1', '$2', <<"player">>, '$3'}, [], [{{'$1','$2','$3'}}]}]),
  PlayersCoordsReduced = [{Session, PlayerCoord, reduce_coords(PlayerCoord, Reduction), PlayerData} || {Session, PlayerCoord, PlayerData} <- Players],
  [{Session, PlayerCoord, PlayerCoordRed, PlayerData} || {Session, PlayerCoord, PlayerCoordRed, PlayerData} <- PlayersCoordsReduced, PlayerCoordRed =:= Coord].
reduce_coords({none, none}, _) ->
  {none, none};
reduce_coords({X, Y}, {XReduction, YReduction}) ->
  TileX = erlang:round(X / XReduction),
  TileY = erlang:round(Y / YReduction),
  {TileX, TileY}.


namespace(Id) ->
	list_to_atom("hs_players_store_" ++ erlang:ref_to_list(Id)).

get_table_name(PlayerStoreHandle) ->
	{TableName} = PlayerStoreHandle,
	TableName.