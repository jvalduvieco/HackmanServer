%% Copyright
-module(hs_messages).
-author("jvalduvieco").

%% API
-export([handle/4]).

handle(<<"login">>, _From = none, _Message, State) ->
	{session_id, SessionId} = hs_game_controller:login(),
	{reply, [{<<"StatusCode">>, 200},
		{<<"type">>,<<"loginResponse">>},
		{<<"sessionId">>, SessionId}], State};
handle(<<"newPlayer">>, From, Message, State) ->
	hs_game_controller:new_player(From, Message),
	{reply, [{<<"statusCode">>, 200},
		{<<"type">>,<<"newPlayerResponse">>}], State};
handle(<<"getPlayers">>, From, _Message, State) ->
	List = hs_player_list:list_players(),
	{reply, [{<<"statusCode">>, 200},
		{<<"type">>,<<"getPlayersResponse">>},
		{<<"list">>, List}],
		State};
handle(<<"positionUpdate">>, From, Message, State)->
	ok = hs_game_controller:position_update(From, Message),
	{noreply, none, State};
handle( none, From, Message, State) ->
	{reply, [{<<"statusCode">>, 400},
		{<<"type">>,<<"errorResponse">>},
		{<<"description">>, <<"Unknown message type.">>},
		{<<"originalMessage">>, Message}],
		{<<"session">>, From},
		State}.