-module(tgws_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
%% {Host, list({Path, Handler, Opts})}
%% Dispatch the requests (whatever the host is) to
%% erws_handler, without any additional options.
	Dispatch = [{'_', [
		{'_', tgws_handler, []}
	]}],

	lager:set_loglevel(lager_console_backend, debug),
	gen_event:start_link({local, tgws_game_event_manager}),
	tgws_root_sup:start_link(),

%% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
%% Listen in 10100/tcp for http connections.
	cowboy:start_listener(tgws_websocket, 5,
		cowboy_tcp_transport, [{port, 10100}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	).

stop(_State) ->
	ok.
