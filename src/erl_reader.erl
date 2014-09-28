-module(erl_reader).
-behaviour(gen_server).

-export([start/0, start_link/0, add_feed/2, import_feed_list/2, list_feeds/0, update/0, update_feed/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erl_reader.hrl").
-include("deps/atomizer/src/atomizer.hrl").
-include("deps/seymour/src/seymour.hrl").

-record(state, {feeds=#{}}).

-define(INTERVAL, 5000).

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
    io:format("~p starting~n", [?MODULE]),
    %erlang:send_after(?INTERVAL, self(), check_for_updates),
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

handle_call(update, _From, #state{feeds=Feeds}=State) ->
    check_for_updates(Feeds),
    {reply, ok, State};

handle_call({update_feed, Uri}, _From, #state{feeds=Feeds}=State) ->
    update_feed(Uri, Feeds),
    {reply, ok, State};

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({add, User, Uri}, #state{feeds=Feeds}=State) ->
    {_, UpdatedFeeds} = add_new_feed_for_user(User, Uri, Feeds),
    {noreply, State#state{feeds=UpdatedFeeds}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_for_updates, #state{feeds=Feeds}=State) ->
    io:format("Checking for updates~n"),
    check_for_updates(Feeds),
    erlang:send_after(?INTERVAL, self(), check_for_updates),
    {noreply, State};

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

update() ->
    gen_server:call(?MODULE, update).

update_feed(Uri) ->
    gen_server:call(?MODULE, {update_feed, Uri}).

save_feed(Feed, Feeds) ->
    Uri = Feed#er_feed.feed,
    lists:keystore(Uri, 1, Feeds, {Uri, Feed}).

get_all_feeds(Feeds) ->
    lists:map(fun({_,Feed}) -> Feed end, Feeds).

add_user_to_feed(User, Uri, Feeds) ->
    io:format("Attempting to add user ~p to feed ~p~n", [User, Uri]),
    case maps:is_key(Uri, Feeds) of
        true ->
            io:format("Found feed~n", []),
            Feed = maps:get(Uri, Feeds),
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
            case supervisor:start_child(er_feed_sup, [Uri]) of
                {ok, Pid} ->
                    UpdatedFeeds = maps:put(Uri, Pid, Feeds),
                    {ok, UpdatedFeeds};
                {error, Error} -> {Error, Feeds}
            end
    end.

check_for_updates(Feeds) ->
    NeedingUpdate = lists:filter(fun(Feed) -> Feed#er_feed.nextCheck < calendar:universal_time() end, get_all_feeds(Feeds)),
    io:format("Feeds needing update: ~p~n", [length(NeedingUpdate)]),
    lists:foreach(fun(Feed) -> supervisor:start_child(er_crawler_sup, [Feed]) end, NeedingUpdate).

update_feed(Uri, Feeds) ->
    [Feed] = lists:filter(fun(Feed) -> Feed#er_feed.feed =:= Uri end, get_all_feeds(Feeds)),
    io:format("Updating feed: ~p~n", [Uri]),
    supervisor:start_child(er_crawler_sup, [Feed]).
