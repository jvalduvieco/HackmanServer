%% Copyright
-module(hs_ai_ghost_events_handler).
-author("jvalduvieco").

-behaviour(gen_event).

%% gen_event
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
	code_change/3]).

%% gen_event callbacks
-record(state, {ai_pid = none}).

init(AiPid) ->
	{ok, #state{ai_pid = AiPid}}.

handle_event({start_match}, State) ->
	hs_ai_ghost:start_match(State#state.ai_pid),
	{ok, State};
handle_event({end_match}, State) ->
	hs_ai_ghost:end_match(State#state.ai_pid),
	{ok, State};
handle_event(_Event, State) ->
	{ok, State}.

handle_call(_Request, State) ->
	{ok, reply, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Arg, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
