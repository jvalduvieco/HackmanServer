-module(hs_websocket_handler).
-behaviour(cowboy_websocket_handler).

% Behaviour cowboy_http_handler
-export([init/3, handle/2, terminate/2]).

% Behaviour cowboy_http_websocket_handler
-export([
	websocket_init/3, websocket_handle/3,
	websocket_info/3, websocket_terminate/3
]).

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
	%%lager:debug("Received: ~p ~p", [self(), Msg]),
	Decoded = jsx:decode(Msg),
	%%lager:debug("Decoded: ~p",[Decoded]),
	{Type, Session} = get_metadata(Decoded),
	{Action, Response, NewState} = hs_messages:handle(Type, {Session, self()}, remove_metadata(Decoded), State),
	translate_to_websocket_response(Action, Response, Req, NewState);

% With this callback we can handle other kind of
% messages, like binary.
websocket_handle(Msg, Req, State) ->
	lager:debug("Received binary: ~p", [Msg]),
	{ok, Req, State}.

% Other messages from the inner system are handled here.
websocket_info(Info, Req, State) ->
	{Action, Data} = Info,
%% FIXME: Fa pudor passar el Req només per a posar-lo a la resposta...
	translate_to_websocket_response(Action, Data, Req, State).

websocket_terminate(_Reason, _Req, _State) ->
	ok.

%% Tools
translate_to_websocket_response(reply, Data, Req, State) ->
	{reply, {text, jsx:encode(Data)}, Req, State};
translate_to_websocket_response(noreply, _Data, Req, State) ->
	{ok, Req, State};
translate_to_websocket_response(_Action, Response, Req, State) ->
	lager:error("Unhandled response: ~p", [Response]),
	{ok, Req, State}.
get_metadata(Message) ->
	{proplists:get_value(<<"type">>, Message,none),
	proplists:get_value(<<"sessionId">>, Message, none)}.
remove_metadata(Message) ->
	proplists:delete(<<"type">>, Message).