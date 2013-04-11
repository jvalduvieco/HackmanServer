-module(hs_player_store).


%% API
-export([init/0, add_player/3, remove_player/2, list_players/1, update_player_position/3, get_player_position/2]).

%% TODO: Refactor this into a service
%% TODO: Make ETS private
init() ->
% Create a new ETS table, called from supervisor
	Id = make_ref(),
	TableName = namespace(Id),
	ets:new(TableName,[set, public, named_table, {write_concurrency, true},  {read_concurrency, true}]),
	{ok, {TableName}}.
add_player(MapHandle,Session, PlayerData) ->
	TableName = get_table_name(MapHandle),
	ets:insert(TableName,{Session,{none, none},PlayerData}).
remove_player(MapHandle,SessionId) ->
	TableName = get_table_name(MapHandle),
	ets:delete(TableName, SessionId).
update_player_position(MapHandle, Key, Position) ->
	TableName = get_table_name(MapHandle),
	ets:update_element(TableName, Key, {2, Position}).
get_player_position(MapHandle, Key) ->
	TableName = get_table_name(MapHandle),
	[MatchResult] = ets:select(TableName, [{{Key, '$2', '_'},[],['$2']}]),
	MatchResult.
list_players(MapHandle) ->
	TableName = get_table_name(MapHandle),
	ets:match(TableName, {'_','$1'}).

namespace(Id) ->
	list_to_atom("hs_players_store_" ++ erlang:ref_to_list(Id)).

get_table_name(MapHandle) ->
	{TableName} = MapHandle,
	TableName.