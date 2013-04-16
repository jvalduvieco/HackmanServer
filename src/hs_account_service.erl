%% Copyright
-module(hs_account_service).
-author("jvalduvieco").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).
-export([login/0]).

-record(state, {next_session_id = 0}).

%% API
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login() ->
	gen_server:call( ?MODULE, {login}).

%% gen_server callbacks
init(_Args) ->
	{ok, #state{}}.

handle_call({login}, _From, State) ->
	% FIXME Maybe move session handling to a session service
	UserId = 33,
	{reply, {ok, State#state.next_session_id, UserId}, State#state{next_session_id = State#state.next_session_id +1}}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
