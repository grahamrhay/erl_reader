-module(er_crawler).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {feed}).

start_link(Feed) ->
    gen_server:start_link(?MODULE, [Feed], []).

init([Feed]) ->
    io:format("~p starting~n", [?MODULE]),
    {ok, #state{feed=Feed}, 0}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    io:format("timeout~n"),
    {stop, normal, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
