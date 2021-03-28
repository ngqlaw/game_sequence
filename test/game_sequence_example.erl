-module(game_sequence_example).

-behaviour(gen_server).

-export([start_link/1, handle/3]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

handle(_Events, _TimeMark, State) ->
    {ok, State}.

init(_Opts) ->
    {ok, ok}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(stop, State) ->
    {stop, normal, State};
handle_info(_Events, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
