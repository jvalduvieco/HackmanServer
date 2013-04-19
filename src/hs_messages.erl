%% Copyright
-module(hs_messages).
-author("jvalduvieco").

%% API
-export([handle/4]).
-record(state, {
	match_referee_pid = none
}).

handle(<<"login">>, _From, _Message, State) ->
	{ok, SessionId, _UserId} = hs_account_service:login(),
	{reply, [{<<"StatusCode">>, 200},
		{<<"type">>,<<"loginResponse">>},
		{<<"data">>,[{<<"sessionId">>, SessionId}]}], #state{}};
handle(<<"listMatches">>, From, Message, State) ->
	MatchId = proplists:get_value(<<"matchId">>, Message, none),
	Matches = hs_match_manager_service:list_matches(MatchId),
	{reply, [{<<"StatusCode">>, 200},
		{<<"type">>, <<"listMatchesResponse">>},
		{<<"data">>, [{<<"matchList">>, Matches}]}], State};
handle(<<"joinMatch">>, From, Message, State) ->
	MatchId = proplists:get_value(<<"matchId">>, Message, none),
	{ok, MatchRefereePid} = hs_match_manager_service:get_match_handle(MatchId),
	hs_match_referee:join(MatchRefereePid, hs_client_handle:get_session(From)),
	{reply, [{<<"StatusCode">>, 200},
		{<<"type">>, <<"joinMatchResponse">>}], State#state{match_referee_pid=MatchRefereePid}};
handle(<<"newMatch">>, From, Message, State) ->
	GameId = proplists:get_value(<<"gameId">>, Message, none),
	{ok, MatchId, MatchRefereePid} = hs_match_manager_service:new_match(GameId),
	hs_match_referee:join(MatchRefereePid, hs_client_handle:get_session(From)),
	{reply, [{<<"StatusCode">>, 200},
		{<<"type">>, <<"newMatchResponse">>},
		{<<"data">>, [{<<"matchId">>, MatchId}]}], State#state{match_referee_pid = MatchRefereePid}};
handle(<<"newPlayer">>, From, Message, State) ->
	hs_match_referee:new_player(State#state.match_referee_pid, From, Message),
	{reply, [{<<"statusCode">>, 200},
		{<<"type">>,<<"newPlayerResponse">>}], State};
handle(<<"getPlayers">>, _From, _Message, State) ->
	List = hs_match_referee:list_players(State#state.match_referee_pid),
	{reply, [{<<"statusCode">>, 200},
		{<<"type">>,<<"getPlayersResponse">>},
		{<<"data">>,List}],
		State};
handle(<<"getObjects">>, _From, _Message, State) ->
	List = hs_match_referee:get_objects(State#state.match_referee_pid),
	{reply, [{<<"statusCode">>, 200},
		{<<"type">>,<<"getObjectsResponse">>},
		{<<"data">>, List}],
		State};
handle(<<"ping">>, _From, Message, State) ->
	{reply, [{<<"statusCode">>, 200},
		{<<"type">>,<<"pong">>}, {<<"data">>, Message}],
		State};
handle(<<"positionUpdate">>, Session, Message, State)->
	ok = hs_match_referee:position_update(State#state.match_referee_pid, Session, Message),
	{noreply, none, State};
handle(<<"pickObject">>, From, Message, State)->

	ok = hs_match_referee:pick_object(State#state.match_referee_pid,From, Message),
	{noreply, none, State};
handle( none, From, Message, State) ->
	{reply, [{<<"statusCode">>, 400},
		{<<"type">>,<<"errorResponse">>},
		{<<"description">>, <<"Unknown message type.">>},
		{<<"originalMessage">>, Message}],
		{<<"session">>, From},
		State}.
