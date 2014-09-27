-module(er_crawler).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("erl_reader.hrl").

-record(state, {feed}).

start_link(Feed) ->
    gen_server:start_link(?MODULE, [Feed], []).

init([Feed]) ->
    io:format("~p starting~n", [?MODULE]),
    gen_server:cast(self(), check_for_updates),
    {ok, #state{feed=Feed}}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(check_for_updates, State = #state{feed=Feed}) ->
    Uri = Feed#er_feed.feed,
    io:format("Checking for updates: ~p~n", [Uri]),
    try atomizer:parse_url(Uri) of
        _Feed ->
            {stop, normal, State}
    catch
        _:Reason ->
            io:format("Error parsing feed: ~p~p~n", [Reason, erlang:get_stacktrace()]),
            {stop, feed_error, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
