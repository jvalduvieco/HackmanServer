-module(hackman_server_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	lager:set_loglevel(lager_console_backend, hs_config:get(log_level)),

	Dispatch = cowboy_router:compile([
		{'_', [
			{'_', hs_websocket_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http,  hs_config:get(start_listeners),
		[{port, hs_config:get(server_port)}],
		[{env, [{dispatch, Dispatch}]}]),

	hs_root_sup:start_link().

stop(_State) ->
	ok.
