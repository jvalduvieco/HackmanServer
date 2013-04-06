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
	lager:debug("Rules enforcement registered ~p ", [Args]),
	Map = hs_file_map_loader:load("priv/level0.json"),
	{ok, #state{map=Map}}.

handle_event({pick_object, ClientData, Data}, State) ->
	ObjectId = proplists:get_value(<<"objectId">>, Data),
	ObjectType = proplists:get_value(<<"objectType">>, Data),
	maybe_give_points(ObjectType, hs_map_store:delete_entity_check_type(State#state.map, ObjectId, ObjectType), ClientData),
	{ok, State};

handle_event(_Event, State) ->
	{ok, State}.

handle_call(_Request, State) ->
	{ok, reply, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Arg, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


maybe_give_points(_ObjectType, NumberOfObjects, Player) when NumberOfObjects =:= 0 ->
	lager:info("A player picked an already picked object. {player:~p, Objects: ~p", [Player, NumberOfObjects]);
maybe_give_points(<<"dots">>, _NumberOfObjects, {_Session, ClientGatewayPid}) ->
	ClientGatewayPid ! {reply, [{<<"type">>, <<"updatePointsAnnounce">>}, {<<"amount">>, 100}]};
maybe_give_points(<<"magicdots">>, _NumberOfObjects, {_Session, ClientGatewayPid}) ->
	ClientGatewayPid ! {reply, [{<<"type">>, <<"updatePointsAnnounce">>}, {<<"amount">>, 500}]}.