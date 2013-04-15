%% Copyright
-module(hs_messages).
-author("jvalduvieco").

%% API
-export([handle/4]).

handle(<<"login">>, _From, _Message, State) ->
	{session_id, SessionId} = hs_game_controller:login(),
	{reply, [{<<"StatusCode">>, 200},
		{<<"type">>,<<"loginResponse">>},
		{<<"sessionId">>, SessionId}], State};
handle(<<"newPlayer">>, From, Message, State) ->
	hs_game_controller:new_player(From, Message),
	{reply, [{<<"statusCode">>, 200},
		{<<"type">>,<<"newPlayerResponse">>}], State};
handle(<<"getPlayers">>, _From, _Message, State) ->
	List = hs_game_controller:list_players(),
	{reply, [{<<"statusCode">>, 200},
		{<<"type">>,<<"getPlayersResponse">>},
		{<<"data">>,List}],
		State};
handle(<<"getObjects">>, _From, _Message, State) ->
	List = hs_game_controller:get_objects(),
	{reply, [{<<"statusCode">>, 200},
		{<<"type">>,<<"getObjectsResponse">>},
		{<<"data">>, List}],
		State};
handle(<<"ping">>, _From, Message, State) ->
	{reply, [{<<"statusCode">>, 200},
		{<<"type">>,<<"pong">>}, {<<"data">>, Message}],
		State};
handle(<<"positionUpdate">>, Session, Message, State)->
	ok = hs_game_controller:position_update(Session, Message),
	{noreply, none, State};
handle(<<"pickObject">>, From, Message, State)->
	ok = hs_game_controller:pick_object(From, Message),
	{noreply, none, State};
handle( none, From, Message, State) ->
	{reply, [{<<"statusCode">>, 400},
		{<<"type">>,<<"errorResponse">>},
		{<<"description">>, <<"Unknown message type.">>},
		{<<"originalMessage">>, Message}],
		{<<"session">>, From},
		State}.
