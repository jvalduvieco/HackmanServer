%% Copyright
-module(hs_client_handle).
-author("jvalduvieco").

%% API
-export([init/2, get_session/1, get_gateway_pid/1]).

init(Session, Pid) ->
	{Session, Pid}.
get_session(ClientData) ->
	{Session, _Pid} = ClientData,
	Session.
get_gateway_pid(ClientData) ->
	{_Session, Pid} =ClientData,
	Pid.