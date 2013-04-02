-module(map_util_test).
-author("jllonch").

-compile(export_all).

% Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").

before_suite() ->
  ok.

before_test() ->
  ok.

test_initialize() ->
  {ok, _Map} = hs_map_util:initialize().

test_insert() ->
  {ok, Map} = hs_map_util:initialize(),
  Id = 1,
  Entity = {test, entity, {1, 2, 3}},
  X = 10,
  Y = 20,
  IsSolid = true,
  {ok, ResultAdd} = hs_map_util:add_entity(Map, Id, Entity, {X, Y}, IsSolid),
  %io:format("~p~n", [ResultAdd]),
  ?assert_equal({{X,Y},{Id,Entity,true}}, ResultAdd),

  {ok, ResultGet} = hs_map_util:get_entities(Map, {X, Y}),
  ?assert_equal([{{X,Y},{Id,Entity,true}}], ResultGet),
  %io:format("~p~n", [ResultGet]).

  Id2 = 2,
  Entity2 = {test, entity2, {4, 5, 6}},
  {ok, ResultAdd2} = hs_map_util:add_entity(Map, Id2, Entity2, {X, Y}, IsSolid),
  ?assert_equal({{X,Y},{Id2,Entity2,true}}, ResultAdd2),

  {ok, ResultGet2} = hs_map_util:get_entities(Map, {X, Y}),
  ?assert_equal([{{X,Y},{Id,Entity,true}}, {{X,Y},{Id2,Entity2,true}}], ResultGet2),




%%   ?assert_equal(1, length( myapp_users:all() )).

%% test_creating_a_new_user() ->
%%   Old = myapp_users:first(),
%%   New = myapp_users:create(
%%     [{name, "Peter"}, {favorite_test_framework, "etest"}]),
%%
%%   ?assert_equal(2, length( myapp_users:all() )),
%%   ?assert_not_equal(Old, New),
%%   ?assert_equal(New, myapp_users:last()).

after_test() ->
  ok.

after_suite() ->
  ok.