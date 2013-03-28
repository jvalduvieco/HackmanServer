-module(hs_player_list).


%% API
-export([init/0, add_player/2, remove_player/1, list_players/0]).

init() ->
	ets:new(hs_players_data_store,[set, named_table, {write_concurrency, true}]).
add_player(Session, PlayerData) ->
	ets:insert(hs_players_data_store,{Session,PlayerData}).
remove_player(SessionId) ->
	ets:delete(hs_players_data_store, SessionId).
list_players() ->
	ets:match(hs_players_data_store, {'_','$1'}).