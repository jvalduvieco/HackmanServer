-module(hs_websocket_handler).
-behaviour(cowboy_websocket_handler).

%%% Behaviour cowboy_http_handler
-export([init/3, handle/2, terminate/2]).

%%% Behaviour cowboy_http_websocket_handler
-export([
	websocket_init/3, websocket_handle/3,
	websocket_info/3, websocket_terminate/3
]).
%%% ========================================
%%% WARN state is defined in hs_messages.erl
%%% ========================================

% Called to know how to dispatch a new connection.
init({tcp, http}, _Req, _Opts) ->
%	lager:debug("Request: ~p", [Req]),
% "upgrade" every request to websocket,
% we're not interested in serving any other content.
	{upgrade, protocol, cowboy_websocket}.

% Should never get here.
handle(Req, State) ->
	lager:debug("Unexpected request: ~p", [Req]),
	{ok, Req2} = cowboy_http_req:reply(404, [
		{'Content-Type', <<"text/html">>}
	]),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.

% Called for every new websocket connection.
websocket_init(_Any, Req, []) ->
	lager:debug("New client"),
	%Req2 = cowboy_http_req:compact(Req),
	{ok, Req, undefined, hibernate}.

% Called when a text message arrives.
websocket_handle({text, Msg}, Req, State) ->
	lager:debug("(~p) ==> ~p", [self(),Msg]),
	Decoded = jsx:decode(Msg),
	%%lager:debug("Decoded: ~p",[Decoded]),
	{Type, Session} = get_metadata(Decoded),
	Data = get_data(Decoded),
	{Action, Response, NewState} = hs_messages:handle(Type, hs_client_handle:init(Session, self()), Data, State),
	handle_result(Action, Response, Req, NewState);

% With this callback we can handle other kind of
% messages, like binary.
websocket_handle(Msg, Req, State) ->
	lager:debug("Received binary: ~p", [Msg]),
	{ok, Req, State}.

% Other messages from the inner system are handled here.
websocket_info(Info, Req, State) ->
	{Action, Data} = Info,
%% FIXME: Fa pudor passar el Req només per a posar-lo a la resposta...
	handle_result(Action, Data, Req, State).

websocket_terminate(_Reason, _Req, _State) ->
	ok.

%% Tools
handle_result(reply, Data, Req, State) ->
	lager:debug("(~p) <== ~p", [self(),Data]),
	{reply, {text, jsx:encode(Data)}, Req, State};
handle_result(noreply, _Data, Req, State) ->
	{ok, Req, State};
handle_result(_Action, Response, Req, State) ->
	lager:error("Unhandled response: ~p", [Response]),
	{ok, Req, State}.

get_metadata(Message) ->
	{proplists:get_value(<<"type">>, Message,none),
	proplists:get_value(<<"sessionId">>, Message, none)}.
%remove_metadata(Message) ->
%	proplists:delete(<<"type">>, Message).
get_data(Message) ->
	proplists:get_value(<<"data">>, Message, none).