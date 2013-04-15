%% Copyright
-module(hs_rules_enforcement).
-author("jvalduvieco").

-behaviour(gen_event).

%% gen_event
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
	code_change/3]).

%% gen_event callbacks
-record(state, {map}).

init(Args) ->
	lager:info("Rules enforcement registered ~p ", [Args]),
	Map = hs_file_map_loader:load(hs_config:get(map_file)),
	{ok, #state{map=Map}}.

handle_event({pick_object, ClientHandle, Data}, State) ->
	ObjectId = proplists:get_value(<<"objectId">>, Data),
	ObjectType = proplists:get_value(<<"objectType">>, Data),
	maybe_give_points(ObjectType, hs_map_store:delete_entity_check_type(State#state.map, ObjectId, ObjectType), ClientHandle),
	{ok, State};

handle_event(_Event, State) ->
	%lager:debug("hs_rules_enforcement: handle_event ~p ..",[Event]),
	{ok, State}.

handle_call(_Request, State) ->
	lager:debug("hs_rules_enforcement: handle_call.."),
	{ok, reply, State}.

handle_info(_Info, State) ->
	lager:debug("hs_rules_enforcement: handle_info.."),
	{ok, State}.

terminate(_Arg, _State) ->
	lager:debug("hs_rules_enforcement: terminate.."),
	ok.

code_change(_OldVsn, State, _Extra) ->
	lager:debug("hs_rules_enforcement: code_change.."),
	{ok, State}.


maybe_give_points(_ObjectType, NumberOfObjects, Player) when NumberOfObjects =:= 0 ->
	lager:info("A player picked an already picked object. {player:~p, Objects: ~p", [Player, NumberOfObjects]);
maybe_give_points(<<"dots">>, _NumberOfObjects, ClientHandle) ->
	hs_client_handle:get_gateway_pid(ClientHandle) ! {reply, [{<<"type">>, <<"updatePointsAnnounce">>}, {<<"data">>, [{<<"amount">>, 100}]}]};
maybe_give_points(<<"magicdots">>, _NumberOfObjects, ClientHandle) ->
	hs_client_handle:get_gateway_pid(ClientHandle) ! {reply, [{<<"type">>, <<"updatePointsAnnounce">>},
		{<<"data">>,[{<<"amount">>, 500}]}]}.