%% Copyright
-module(hs_events_handler).
-author("jvalduvieco").

-behaviour(gen_event).

%% gen_event
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state,{session_id = none, websocket_pid = none}).
%% gen_event callbacks
init(Parameters) ->
	lager:debug("User event handler registered ~p ", [Parameters]),
	SessionId = proplists:get_value(session_id, Parameters, none),
	WebsocketPid = proplists:get_value(websocket_pid, Parameters, none),
	{ok, #state{session_id = SessionId, websocket_pid = WebsocketPid}}.

handle_event({position_update, ClientHandle, Data}, State) ->
	MySession = State#state.session_id,
	Session = hs_client_handle:get_session(ClientHandle),
	case Session of
		MySession ->
			ok;
		_OtherSessionId ->
			State#state.websocket_pid ! {reply, [{<<"type">>, <<"positionUpdateAnnounce">>}, {<<"data">>, Data}]}
	end,
	{ok, State};
handle_event({pick_object, ClientHandle, Data}, State) ->
	MySession = State#state.session_id,
	Session = hs_client_handle:get_session(ClientHandle),
	case Session of
		MySession ->
			ok;
		_OtherSessionId ->
			State#state.websocket_pid ! {reply, [{<<"type">>, <<"pickObjectAnnounce">>}, {<<"data">>,  Data}]}
	end,
	{ok, State};
handle_event({new_player,ClientHandle, PlayerData}, State) ->
	MySession = State#state.session_id,
	Session = hs_client_handle:get_session(ClientHandle),
	case Session of
		MySession ->
			ok;
		_OtherSessionId ->
			State#state.websocket_pid ! {reply,
				[{<<"type">>, <<"newPlayerAnnounce">>}, {<<"data">>, PlayerData}]}
	end,
	{ok, State};
handle_event({start_match}, State) ->
	State#state.websocket_pid ! {reply, [{<<"type">>, <<"startMatch">>}]},
	{ok, State};
handle_event({end_match}, State) ->
	State#state.websocket_pid ! {reply, [{<<"type">>, <<"endMatch">>}]},
	{ok, State};
handle_event(_Event, State) ->
	lager:debug("hs_events_handler: handle_event.."),
	{ok, State}.

handle_call(_Request, State) ->
	lager:debug("hs_events_handler: handle_call.."),
	{ok, reply, State}.

handle_info(_Info, State) ->
	lager:debug("hs_events_handler: handle_info.."),
	{ok, State}.

terminate(_Arg, _State) ->
	lager:debug("hs_events_handler: terminate.."),
	ok.

code_change(_OldVsn, State, _Extra) ->
	lager:debug("hs_events_handler: code_change.."),
	{ok, State}.