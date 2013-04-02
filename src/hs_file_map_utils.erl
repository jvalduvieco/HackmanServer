%% Copyright
-module(hs_file_map_utils).
-author("jvalduvieco").

%% API
-export([load/1, get_property/2, get_layer/2, get_layer_data/1, get_layer_type/1, get_layer_objects/1, load_collision_layer/3]).

load(MapFileName) ->
	{ok, JSONText} = file:read_file(MapFileName),
	jsx:decode(JSONText).

get_property(MapData, PropertyName) ->
	proplists:get_value(PropertyName, MapData, undefined).

get_layer(MapData, LayerName) ->
	Layers = proplists:get_value(<<"layers">>, MapData),
	[Result] = [Layer || Layer <- Layers, proplists:get_value(<<"name">>, Layer) == LayerName],
	Result.

get_layer_type(Layer) ->
	proplists:get_value(<<"type">>, Layer, undefined).

get_layer_data(Layer) ->
	proplists:get_value(<<"data">>, Layer, undefined).

get_layer_objects(Layer) ->
	proplists:get_value(<<"objects">>, Layer, undefined).

load_collision_layer(CollisionLayer, Map, {X, Y}, MaxX, MaxY) when X == MaxX ->
	load_collision_layer(CollisionLayer, Map, {0,Y+1}, MaxX, MaxY);
load_collision_layer(_CollisionLayer, _Map, {_X, Y}, _MaxX, MaxY) when Y == MaxY ->
	ok;
load_collision_layer([H|CollisionLayer], Map, {X, Y}, MaxX, MaxY) when H == 1 ->
	hs_map_util:add_entity(Map, Y*MaxX+X, wall, {X,Y}, true),
	load_collision_layer(CollisionLayer, Map, {X+1,Y}, MaxX, MaxY);
load_collision_layer([H|CollisionLayer], Map, {X, Y}, MaxX,MaxY) when H == 0 ->
	load_collision_layer(CollisionLayer, Map, {X+1,Y}, MaxX, MaxY).

load_collision_layer(CollisionLayer, MaxX, MaxY) ->
	{ok, Map} = hs_map_util:initialize(),
	load_collision_layer(CollisionLayer, Map, {0,0}, MaxX, MaxY).