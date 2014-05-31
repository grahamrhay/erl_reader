-module(erl_reader).
-behaviour(gen_server).

-export([start/0, start_link/0, add_feed/2, import_feed_list/2, list_feeds/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erl_reader.hrl").
-include("deps/atomizer/src/atomizer.hrl").
-include("deps/seymour/src/seymour.hrl").

-record(state, {feeds=[]}).

start() ->
    application:start(inets),
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    {ok, #state{}}.

handle_call({add, User, Uri}, _From, #state{feeds=Feeds}=State) ->
    {Result, UpdatedFeeds} = add_new_feed_for_user(User, Uri, Feeds),
    {reply, Result, State#state{feeds=UpdatedFeeds}};

handle_call({import, User, File}, _From, State) ->
    try seymour:parse_file(File) of
        FeedList ->
            add_user_to_multiple_feeds(User, FeedList),
            {reply, ok, State}
    catch
        throw:Reason -> {reply, Reason, State}
    end;

handle_call(list, _From, #state{feeds=Feeds}=State) ->
    {reply, get_all_feeds(Feeds), State};

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({add, User, Uri}, #state{feeds=Feeds}=State) ->
    {_, UpdatedFeeds} = add_new_feed_for_user(User, Uri, Feeds),
    {noreply, State#state{feeds=UpdatedFeeds}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

add_feed(User, Uri) ->
    gen_server:call(?MODULE, {add, User, Uri}).

import_feed_list(User, File) ->
    gen_server:call(?MODULE, {import, User, File}).

list_feeds() ->
    gen_server:call(?MODULE, list).

create_feed(FeedUri, Feed, User) ->
    #er_feed
    {
        id=uuid:get_v4(),
        feed=FeedUri,
        uri=Feed#feed.url,
        lastUpdated=Feed#feed.updated,
        nextCheck=time_for_next_check(Feed),
        entries=lists:map(fun(FE) -> to_er_entry(FE) end, Feed#feed.entries),
        users=[User]
    }.

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

save_feed(Feed, Feeds) ->
    Uri = Feed#er_feed.feed,
    lists:keystore(Uri, 1, Feeds, {Uri, Feed}).

get_all_feeds(Feeds) ->
    lists:map(fun({_,Feed}) -> Feed end, Feeds).

add_user_to_feed(User, Uri, Feeds) ->
    io:format("Attempting to add user ~p to feed ~p~n", [User, Uri]),
    case lists:keyfind(Uri, 1, Feeds) of
        {Uri, Feed} ->
            io:format("Found feed~n", []),
            case lists:member(User, Feed#er_feed.users) of
                true ->
                    io:format("Already subscribed~n", []),
                    {ok, Feeds};
                false ->
                    io:format("Adding user~n", []),
                    UpdatedFeed = Feed#er_feed{users=[User|Feed#er_feed.users]},
                    UpdatedFeeds = save_feed(UpdatedFeed, Feeds),
                    {ok, UpdatedFeeds}
            end;
        false ->
            io:format("No matching feed~n", []),
            no_match
    end.

add_user_to_multiple_feeds(User, FeedList) ->
    lists:foreach(fun(Feed) -> gen_server:cast(?MODULE, {add, User, Feed#seymour_feed.xmlUrl}) end, FeedList).

add_new_feed_for_user(User, Uri, Feeds) ->
    case add_user_to_feed(User, Uri, Feeds) of
        {ok, UpdatedFeeds} ->
            {ok, UpdatedFeeds};
        no_match ->
            try atomizer:parse_url(Uri) of % TODO: handle redirects
                unknown ->
                    io:format("Unable to parse feed: ~p~n", [Uri]),
                    {bad_feed, Feeds};
                Feed ->
                    io:format("Creating new feed entry for ~p~n", [Uri]),
                    FeedRecord = create_feed(Uri, Feed, User),
                    UpdatedFeeds = save_feed(FeedRecord, Feeds),
                    {ok, UpdatedFeeds}
            catch
                _:Reason ->
                    io:format("Error parsing feed: ~p~p~n", [Reason, erlang:get_stacktrace()]),
                    {Reason, Feeds}
            end
    end.
