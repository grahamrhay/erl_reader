-module(erl_reader).
-behaviour(gen_server).

-export([start/0, start_link/0, add_feed/2, import_feed_list/2]).

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

add_user_to_feed(User, Uri) ->
    gen_server:call(er_user_feeds, {add, User, Uri}).

add_user_to_multiple_feeds(User, FeedList) ->
    lists:foreach(fun(Feed) -> gen_server:cast(?MODULE, {add, User, Feed#seymour_feed.xmlUrl}) end, FeedList).

add_new_feed_for_user(User, Uri, Feeds) ->
    case create_feed(Uri, Feeds) of
        {ok, UpdatedFeeds} ->
            add_user_to_feed(User, Uri),
            {ok, UpdatedFeeds};
        {error, Error} -> {error, Error}
    end.

create_feed(Uri, Feeds) ->
    case maps:is_key(Uri, Feeds) of
        true -> {ok, Feeds};
        false ->
            case supervisor:start_child(er_feed_sup, [Uri]) of
                {ok, Pid} ->
                    UpdatedFeeds = maps:put(Uri, Pid, Feeds),
                    {ok, UpdatedFeeds};
                {error, Error} -> {Error, Feeds}
            end
    end.
