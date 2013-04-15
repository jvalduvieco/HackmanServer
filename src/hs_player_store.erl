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
add_player(UserStoreHandle,Session, PlayerData) ->
	TableName = get_table_name(UserStoreHandle),
	ets:insert(TableName,{Session,{none, none},PlayerData}).
remove_player(UserStoreHandle,SessionId) ->
	TableName = get_table_name(UserStoreHandle),
	ets:delete(TableName, SessionId).
update_player_position(UserStoreHandle, Key, Position) ->
	TableName = get_table_name(UserStoreHandle),
	ets:update_element(TableName, Key, {2, Position}).
get_player_position(UserStoreHandle, Key) ->
	TableName = get_table_name(UserStoreHandle),
	[MatchResult] = ets:select(TableName, [{{Key, '$2', '_'},[],['$2']}]),
	MatchResult.
list_players(UserStoreHandle) ->
	TableName = get_table_name(UserStoreHandle),
	%ets:match(TableName, {'$1','$2','$3'}).
	ets:select(TableName, [{{'$1', '$2', '$3'},[],['$1','$2','$3']}]).

namespace(Id) ->
	list_to_atom("hs_players_store_" ++ erlang:ref_to_list(Id)).

get_table_name(UserStoreHandle) ->
	{TableName} = UserStoreHandle,
	TableName.