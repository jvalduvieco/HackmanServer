%% Copyright
-module(hs_map_util).
-author("jordillonch").

%% API
-export([initialize/0, add_entity/5, delete_entity/2, get_entities/2, is_solid/2, free_move_positions/2]).

%% returns a new map object
initialize() ->
  % Create a new ETS table, called from supervisor
  Id = make_ref(),
  TableName = namespace(Id),
  ets:new(TableName,[bag, public, named_table, {write_concurrency, true}]),
  {ok, {TableName}}.

%% add an entity to the map
%% if you want to creat walls you can add an empty entity that is collidable
add_entity(Map, Id, Entity, {X, Y}, IsSolid) ->
  TableName = get_table_name(Map),
  Key = {X, Y},
  Value = {Id, Entity, IsSolid},
  {ok, Result} = case ets:insert(TableName, {Key, Value}) of
    true ->
      {ok, {Key, Value}};
    {error, Reason} ->
      {error, Reason}
  end,
  {ok, Result}.

delete_entity(Map, Id) ->
  TableName = get_table_name(Map),
  List = ets:match(TableName, {{'_', '_'}, {Id, '_', '_'}}),
  delete_entity(Map, Id, List),
  ok.
delete_entity(_, _, []) ->
  ok;
delete_entity(Map, Id, [H|T]) ->
  TableName = get_table_name(Map),
  {Key, _} = H,
  ok = ets:delete(TableName, Key),
  delete_entity(Map, Id, T).

get_entities(Map, {X, Y}) ->
  TableName = get_table_name(Map),
  Key = {X, Y},
  List = ets:lookup(TableName, Key),
  Result = [Value || {_Key, Value} <- List],
  {ok, Result}.

is_solid(Map, {X, Y}) ->
  {ok, List} = get_entities(Map, {X, Y}),
  Result = [1 || {_, _, IsSolid} <- List, IsSolid =:= true],
  case Result of
    [] -> false;
    _ -> true
  end.

%% get a list of coords that are ok to move an entity because is not colliding with anything
free_move_positions(Map, {X, Y}) ->
  List = [
  {{X,   Y-1}, is_solid(Map, {X,   Y-1})},
  {{X+1, Y-1}, is_solid(Map, {X+1, Y-1})},
  {{X+1, Y},   is_solid(Map, {X+1, Y})},
  {{X+1, Y+1}, is_solid(Map, {X+1, Y+1})},
  {{X,   Y+1}, is_solid(Map, {X,   Y+1})},
  {{X-1, Y+1}, is_solid(Map, {X-1, Y+1})},
  {{X-1, Y},   is_solid(Map, {X-1, Y})},
  {{X-1, Y-1}, is_solid(Map, {X-1, Y-1})}
  ],
  Result = [Coords || {Coords, IsSolid} <- List, IsSolid =:= false],
  {ok, Result}.


namespace(Id) ->
  list_to_atom("hs_map_util_" ++ erlang:ref_to_list(Id)).

get_table_name(Map) ->
  {TableName} = Map,
  TableName.
