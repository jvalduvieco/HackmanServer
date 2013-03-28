-module(hackman_server_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
%% {Host, list({Path, Handler, Opts})}
%% Dispatch the requests (whatever the host is) to
%% erws_handler, without any additional options.
	Dispatch = [{'_', [
		{'_', hs_websocket_handler, []}
	]}],

	lager:set_loglevel(lager_console_backend, info),
	gen_event:start_link({local, hs_game_event_manager}),
	hs_root_sup:start_link(),

%% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
%% Listen in 10100/tcp for http connections.
	cowboy:start_listener(hs_websocket_listener, 5,
		cowboy_tcp_transport, [{port, 10100}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	).

stop(_State) ->
	ok.
