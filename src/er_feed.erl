-module(er_feed).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("erl_reader.hrl").
-include("deps/atomizer/src/atomizer.hrl").

-record(state, {feed}).

start_link(Uri) ->
    gen_server:start_link(?MODULE, [Uri], []).

init([Uri]) ->
    io:format("~p starting. Uri: ~p~n", [?MODULE, Uri]),
    case get_feed(Uri) of
        {ok, Feed} ->
            io:format("Creating new feed entry for ~p~n", [Uri]),
            io:format("Pid~p~n", [self()]),
            FeedRecord = create_feed(Uri, Feed),
            {ok, #state{feed=FeedRecord}};
        {error, Error} ->
            {stop, Error}
    end.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(update, State = #state{feed=Feed}) ->
    Uri = Feed#er_feed.feed,
    io:format("Checking for updates to ~p~n", [Uri]),
    {ok, LatestFeed} = get_feed(Uri),
    check_for_updates(Feed, LatestFeed),
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_feed(Uri) ->
    try atomizer:parse_url(Uri) of % TODO: handle redirects
        unknown ->
            io:format("Unable to parse feed: ~p~n", [Uri]),
            {error, bad_feed};
        Feed ->
            {ok, Feed}
    catch
        _:Reason ->
            io:format("Error parsing feed: ~p~p~n", [Reason, erlang:get_stacktrace()]),
            {error, Reason}
    end.

create_feed(FeedUri, Feed) ->
    #er_feed
    {
        feed=FeedUri,
        uri=Feed#feed.url,
        lastUpdated=get_last_updated(Feed#feed.updated),
        nextCheck=time_for_next_check(Feed),
        entries=lists:map(fun(FE) -> to_er_entry(FE) end, Feed#feed.entries)
    }.

get_last_updated(undefined) ->
    calendar:universal_time();

get_last_updated(LastUpdated) ->
    LastUpdated.

time_for_next_check(_Feed) ->
    %% TODO: check TTL, or freq of updates
    Now = calendar:universal_time(),
    NowInSeconds = calendar:datetime_to_gregorian_seconds(Now),
    OneHourInSeconds = 3600,
    calendar:gregorian_seconds_to_datetime(NowInSeconds + OneHourInSeconds).

to_er_entry(FeedEntry) ->
    #er_entry
    {
        title=FeedEntry#feedentry.title,
        date=FeedEntry#feedentry.date,
        link=FeedEntry#feedentry.permalink,
        content=FeedEntry#feedentry.content
    }.

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
