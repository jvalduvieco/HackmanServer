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

	lager:set_loglevel(lager_console_backend, hs_config:get(log_level)),
	hs_root_sup:start_link(),

%% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
%% Listen in 10100/tcp for http connections.
	cowboy:start_listener(hs_websocket_listener, hs_config:get(start_listeners),
		cowboy_tcp_transport, [{port, hs_config:get(server_port)}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	).

stop(_State) ->
	ok.
