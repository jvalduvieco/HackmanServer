-module(hs_file_map_loader).
-author("jvalduvieco").

%% API
%% Map functions
-export([load/1]).

%-export([get_map_property/2]).
%% Layer functions
%-export([get_layer/2, get_layer_data/1, get_layer_type/1, get_layer_objects/1]).
%% Object functions
%-export([get_object_position/1, get_object_name/1, translate_coords_to_tile/3]).

%% loaders
%-export([load_collision_layer/4, load_object_layer/6]).

decode(MapFileName) ->
	{ok, JSONText} = file:read_file(MapFileName),
	jsx:decode(JSONText).

get_map_property(MapData, PropertyName) ->
	proplists:get_value(PropertyName, MapData, undefined).

get_layer(MapData, LayerName) ->
	Layers = proplists:get_value(<<"layers">>, MapData),
	[Result] = [Layer || Layer <- Layers, proplists:get_value(<<"name">>, Layer) == LayerName],
	Result.

get_layer_data(Layer) ->
	proplists:get_value(<<"data">>, Layer, undefined).

get_layer_objects(Layer) ->
	proplists:get_value(<<"objects">>, Layer, undefined).

get_object_position(Object) ->
	X = proplists:get_value(<<"x">>, Object, undefined),
	Y = proplists:get_value(<<"y">>, Object, undefined),
	{X,Y}.

load_collision_layer(CollisionLayer, Map, {X, Y}, MaxX, MaxY) when X == MaxX ->
	load_collision_layer(CollisionLayer, Map, {0,Y+1}, MaxX, MaxY);
load_collision_layer(_CollisionLayer, Map, {_X, Y}, _MaxX, MaxY) when Y == MaxY ->
	{ok, Map};
load_collision_layer([H|CollisionLayer], Map, {X, Y}, MaxX, MaxY) when H == 1 ->
	BinId = erlang:integer_to_binary( Y*MaxX+X,10),
	IdPrefix = <<"wall">>,
	hs_map_store:add_entity(Map, <<IdPrefix/binary, BinId/binary>>, <<"wall">>, {X,Y}, true),
	load_collision_layer(CollisionLayer, Map, {X+1,Y}, MaxX, MaxY);
load_collision_layer([H|CollisionLayer], Map, {X, Y}, MaxX,MaxY) when H == 0 ->
	load_collision_layer(CollisionLayer, Map, {X+1,Y}, MaxX, MaxY).

load_collision_layer(Map, CollisionLayer, MaxX, MaxY) ->
	load_collision_layer(CollisionLayer, Map, {0, 0}, MaxX, MaxY).

load_object_layer(_Map, [], _ObjectType, _Id, _TileWidth, _TileHeight) ->
	ok;
load_object_layer(Map, ObjectLayer, ObjectType, Id, TileWidth, TileHeight) ->
	[Object|Rest] = ObjectLayer,
	BinId = erlang:integer_to_binary(Id, 10),
	CompletebinId = <<ObjectType/binary, BinId/binary>>,
	hs_map_store:add_entity(Map, CompletebinId, ObjectType, get_object_position(Object), false),
	load_object_layer(Map, Rest, ObjectType, Id+1, TileWidth, TileHeight).

load(FileName) ->
	{ok, MapStore} = hs_map_store:init(),
	MapData = decode(FileName),
	MapHeight = get_map_property(MapData, <<"height">>),
	MapWidth = get_map_property(MapData, <<"width">>),
	TileHeight = get_map_property(MapData, <<"tileheight">>),
	TileWidth = get_map_property(MapData, <<"tilewidth">>),
	CollisionLayer = get_layer(MapData, <<"collision">>),
	CollisionLayerData = get_layer_data(CollisionLayer),
	load_collision_layer(MapStore, CollisionLayerData, MapWidth, MapHeight),
	DotLayer = get_layer(MapData,<<"dots">>),
	DotLayerData = get_layer_objects(DotLayer),
	load_object_layer(MapStore, DotLayerData, <<"dots">>, 0, TileWidth, TileHeight),
	MagicDotLayer = get_layer(MapData,<<"magicdots">>),
	MagicDotLayerData = get_layer_objects(MagicDotLayer),
	load_object_layer(MapStore, MagicDotLayerData, <<"magicdots">>, 0, TileWidth, TileHeight),
	MapStore.