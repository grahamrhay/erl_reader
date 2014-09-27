-module(er_crawler).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("erl_reader.hrl").
-include("deps/atomizer/src/atomizer.hrl").

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
        UpdatedFeed ->
            check_for_updates(Feed, UpdatedFeed),
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

check_for_updates(Feed, UpdatedFeed) when UpdatedFeed#feed.updated =/= undefined, Feed#er_feed.lastUpdated >= UpdatedFeed#feed.updated ->
    % no-op if last update for feed is as old or older than the version we have already
    ok;

check_for_updates(Feed, UpdatedFeed) ->
    NewEntries = lists:filter(fun(Entry) ->
                                  MatchingEntries = lists:filter(fun(E) -> E#er_entry.link =:= Entry#feedentry.permalink end, Feed#er_feed.entries),
                                  length(MatchingEntries) =:= 0
                              end, UpdatedFeed#feed.entries),
    io:format("New entries: ~p~n", [NewEntries]),
    ok.
