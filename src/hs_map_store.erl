%% Copyright
-module(hs_map_store).
-author("jordillonch").

%% API
-export([init/4, add_entity/5, set/3, set_properties/5, delete_entity_check_type/3, get_entities/2,
	get_entities_by_type/2, is_void/2, exists/2, free_move_positions/2]).

%% TODO: Refactor this into a service
%% TODO: Make ETS private
-record(map_handle, {
	table_name = none,
	max_x = none,
	max_y = none,
	tile_height = none,
	tile_width = none
}).
%% returns a new map object
init(MaxX, MaxY, TileHeight, TileWidth) ->
  % Create a new ETS table, called from supervisor
  Id = make_ref(),
  TableName = namespace(Id),
  ets:new(TableName,[bag, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
  {ok, #map_handle{table_name = TableName, max_x = MaxX, max_y = MaxY, tile_height = TileHeight, tile_width = TileWidth}}.

%% TODO: Add map properties as a proplist into the handle
set(MapHandle, max_x, Value) ->
	MapHandle#map_handle{max_x = Value};
set(MapHandle, max_y, Value) ->
	MapHandle#map_handle{max_y = Value};
set(MapHandle, tile_height, Value) ->
	MapHandle#map_handle{tile_height = Value};
set(MapHandle, tile_width, Value) ->
	MapHandle#map_handle{tile_width = Value}.

set_properties (MapHandle, MaxX, MaxY, TileHeight, TileWidth) ->
	MapHandle#map_handle{max_x = MaxX, max_y = MaxY, tile_height = TileHeight, tile_width = TileWidth}.
%% add an entity to the map
%% if you want to creat walls you can add an empty entity that is collidable
add_entity(MapHandle, Id, Entity, {X, Y}, IsSolid) ->
  TableName = get_table_name(MapHandle),
  Key = {X, Y},
  Value = {Id, Entity, IsSolid},
  {ok, Result} = case ets:insert(TableName, {Key, Value}) of
    true ->
      {ok, {Key, Value}};
    {error, Reason} ->
      {error, Reason}
  end,
  {ok, Result}.

delete_entity_check_type(MapHandle, Id, Type) ->
  TableName = get_table_name(MapHandle),
	ets:select_delete(TableName, [{{{'_','_'}, {Id, Type, '_'}}, [], [true]}]).

get_entities(MapHandle, {X, Y}) ->
  TableName = get_table_name(MapHandle),
  Key = {X, Y},
  List = ets:lookup(TableName, Key),
  Result = [Value || {_Key, Value} <- List],
  {ok, Result}.

get_entities_by_type(MapHandle, Type) ->
	TableName = get_table_name(MapHandle),
	MatchResult = ets:select(TableName, [{{{'$1','$2'},{'$3',Type,'_'}},[],[['$1','$2','$3']]}]),
	Result =[[{<<"x">>, X}, {<<"y">>, Y}, {<<"id">>, Id}, {<<"type">>, Type}]||[X,Y,Id] <- MatchResult],
	{ok, Result}.

is_void(MapHandle, Coords) ->
  {ok, List} = get_entities(MapHandle, Coords),
  Result = [1 || {_, _, IsSolid} <- List, IsSolid =:= true],
%%   lager:debug("[is_void]: (~p) List: ~p; Result: ~p", [Coords, List, Result]),
  case Result of
    [] -> false;
    _ -> true
  end.

exists(MapHandle, {X, Y}) when X >= 0 andalso X =< MapHandle#map_handle.max_x andalso Y >= 0 andalso Y =< MapHandle#map_handle.max_y ->
	true;
exists(_MapHandle, {_X,_Y}) ->
	false.
check_suitability(MapHandle, Coords) ->
  Exists = exists(MapHandle, Coords),
  IsVoid = is_void(MapHandle, Coords),
%%   lager:debug("check_suitability: (~p) ~p andalso ~p", [Coords, Exists, IsVoid]),
  Exists andalso IsVoid.

%% get a list of coords that are ok to move an entity because is not colliding with anything
%% orthogonal
free_move_positions(MapHandle, {X, Y}) ->
  List = [
  {{X,   Y-1}, check_suitability(MapHandle, {X,   Y-1})},
%%   {{X+1, Y-1}, is_solid(MapHandle, {X+1, Y-1})},
  {{X+1, Y},   check_suitability(MapHandle, {X+1, Y})},
%%   {{X+1, Y+1}, is_solid(MapHandle, {X+1, Y+1})},
  {{X,   Y+1}, check_suitability(MapHandle, {X,   Y+1})},
%%   {{X-1, Y+1}, is_solid(MapHandle, {X-1, Y+1})},
  {{X-1, Y},   check_suitability(MapHandle, {X-1, Y})}
%%   {{X-1, Y-1}, is_solid(MapHandle, {X-1, Y-1})}
  ],
  Result = [Coords || {Coords, IsSuitable} <- List, IsSuitable =:= false],
  {ok, Result}.

namespace(Id) ->
  list_to_atom("hs_map_store_" ++ erlang:ref_to_list(Id)).

get_table_name(MapHandle) ->
	MapHandle#map_handle.table_name.
