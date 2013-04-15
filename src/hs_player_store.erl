-module(hs_player_store).


%% API
-export([init/0, add_player/4, remove_player/2, list_players/1, list_players_by_type/2, update_player_position/3, get_player_position/2]).

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
remove_player(PlayerStoreHandle,SessionId) ->
	TableName = get_table_name(PlayerStoreHandle),
	ets:delete(TableName, SessionId).
update_player_position(PlayerStoreHandle, Key, Position) ->
	TableName = get_table_name(PlayerStoreHandle),
	ets:update_element(TableName, Key, {2, Position}).
get_player_position(PlayerStoreHandle, Key) ->
	TableName = get_table_name(PlayerStoreHandle),
	% FIXME replace by a lookup?
	ets:lookup_element(TableName, Key ,2).
list_players(PlayerStoreHandle) ->
	TableName = get_table_name(PlayerStoreHandle),
	ets:select(TableName, [{{'_', '_', '_', '$1'}, [], ['$1']}]).
list_players_by_type(PlayerStoreHandle, PlayerType) ->
	TableName = get_table_name(PlayerStoreHandle),
	ets:select(TableName, [{{'$1', '$2', PlayerType, '$3'}, [], ['$3']}]).

namespace(Id) ->
	list_to_atom("hs_players_store_" ++ erlang:ref_to_list(Id)).

get_table_name(PlayerStoreHandle) ->
	{TableName} = PlayerStoreHandle,
	TableName.