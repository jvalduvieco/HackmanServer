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

handle_event({position_update, {Session, _ClientGatewayPid}, Data}, State) ->
	MySession = State#state.session_id,
	lager:debug("Check S:~p M:~p",[Session, MySession]),
	case Session of
		MySession ->
			ok;
		_OtherSessionId ->
			State#state.websocket_pid ! {reply, [{<<"type">>, <<"positionUpdateAnnounce">>} | Data]}
	end,
	{ok, State};
handle_event({pick_object, {Session, _ClientGatewayPid}, Data}, State) ->
	MySession = State#state.session_id,
	case Session of
		MySession ->
			ok;
		_OtherSessionId ->
			State#state.websocket_pid ! {reply, [{<<"type">>, <<"pickObjectAnnounce">>} | Data]}
	end,
	{ok, State};
handle_event({new_player, {Session, _ClientGatewayPid}, PlayerData}, State) ->
	MySession = State#state.session_id,
	case Session of
		MySession ->
			ok;
		_OtherSessionId ->
			State#state.websocket_pid ! {reply, [{<<"type">>, <<"newPlayerAnnounce">>},{<<"sessionId">>, Session}] ++ PlayerData}
	end,
	{ok, State};
handle_event({start_game}, State) ->
	lager:debug("Start game received ~p", [State]),
	State#state.websocket_pid ! {reply, [{<<"type">>, <<"startGame">>}]},
	{ok, State};
handle_event(Event, State) ->
	lager:error("Unhandled event: ~p", [Event]),
	{ok, State}.

handle_call(Request, State) ->
	lager:error("Unhandled call: ~p", [Request]),
	{ok, reply, State}.

handle_info(Info, State) ->
	lager:error("Unhandled info: ~p", [Info]),
	{ok, State}.

terminate(_Arg, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.