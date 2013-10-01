-module(erl_reader).
-behaviour(gen_server).

-export([start/0, start_link/0, add_feed/2, list_feeds/0, create_db/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erl_reader.hrl").
-include("deps/atomizer/src/atomizer.hrl").

-record(state, {feeds=[]}).

start() ->
    application:start(inets),
    application:start(mnesia),
    application:start(?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    {ok, #state{}}.

handle_call({add, User, Uri}, _From, State) ->
    case add_user_to_feed(User, Uri) of
        ok ->
            {reply, ok, State};
        no_match ->
            try atomizer:parse_url(Uri) of % TODO: handle redirects
                unknown ->
                    {reply, bad_feed, State};
                Feed ->
                    save_feed(create_feed(Uri, Feed, User)),
                    {reply, ok, State}
            catch
                throw:Reason -> {reply, Reason, State}
            end
    end;

handle_call(list, _From, State) ->
    {reply, get_all_feeds(), State};

handle_call(_, _From, State) ->
    {reply, ok, State}.

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

save_feed(Feed) ->
    mnesia:activity(transaction, fun() -> mnesia:write(Feed) end).

get_all_feeds() ->
    mnesia:activity(transaction,fun() -> mnesia:select(er_feed, [{'_',[],['$_']}]) end). 

add_user_to_feed(User, Uri) ->
    io:format("Attempting to add user ~p to feed ~p~n", [User, Uri]),
    mnesia:activity(transaction, fun() ->
        case mnesia:index_read(er_feed, Uri, #er_feed.feed) of
            [Feed] ->
                io:format("Found feed~n", []),
                case lists:member(User, Feed#er_feed.users) of
                    true ->
                        io:format("Already subscribed~n", []),
                        ok;
                    false ->
                        io:format("Adding user~n", []),
                        UpdatedFeed = Feed#er_feed{users=[User|Feed#er_feed.users]},
                        mnesia:write(UpdatedFeed),
                        ok
                end;
            [] ->
                io:format("No matching feed~n", []),
                no_match
        end
    end).

create_db() ->
    Nodes = [node()],
    mnesia:create_schema(Nodes),
    application:start(mnesia),
    mnesia:create_table(er_feed, [
        {attributes, record_info(fields, er_feed)},
        {index, [#er_feed.feed]},
        {disc_copies, Nodes}
    ]),
    application:stop(mnesia).
