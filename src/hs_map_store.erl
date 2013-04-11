%% Copyright
-module(hs_map_store).
-author("jordillonch").

%% API
-export([init/0, add_entity/5, delete_entity_check_type/3, get_entities/2,
	get_entities_by_type/2, is_solid/2, free_move_positions/2]).

%% TODO: Refactor this into a service
%% TODO: Make ETS private

%% returns a new map object
init() ->
  % Create a new ETS table, called from supervisor
  Id = make_ref(),
  TableName = namespace(Id),
  ets:new(TableName,[bag, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
  {ok, {TableName}}.

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

is_solid(MapHandle, {X, Y}) ->
  {ok, List} = get_entities(MapHandle, {X, Y}),
  Result = [1 || {_, _, IsSolid} <- List, IsSolid =:= true],
  case Result of
    [] -> false;
    _ -> true
  end.

%% get a list of coords that are ok to move an entity because is not colliding with anything
%% orthogonal
free_move_positions(MapHandle, {X, Y}) ->
  List = [
  {{X,   Y-1}, is_solid(MapHandle, {X,   Y-1})},
%%   {{X+1, Y-1}, is_solid(MapHandle, {X+1, Y-1})},
  {{X+1, Y},   is_solid(MapHandle, {X+1, Y})},
%%   {{X+1, Y+1}, is_solid(MapHandle, {X+1, Y+1})},
  {{X,   Y+1}, is_solid(MapHandle, {X,   Y+1})},
%%   {{X-1, Y+1}, is_solid(MapHandle, {X-1, Y+1})},
  {{X-1, Y},   is_solid(MapHandle, {X-1, Y})}
%%   {{X-1, Y-1}, is_solid(MapHandle, {X-1, Y-1})}
  ],
  Result = [Coords || {Coords, IsSolid} <- List, IsSolid =:= false],
  {ok, Result}.

namespace(Id) ->
  list_to_atom("hs_map_store_" ++ erlang:ref_to_list(Id)).

get_table_name(MapHandle) ->
	{TableName} = MapHandle,
	TableName.
