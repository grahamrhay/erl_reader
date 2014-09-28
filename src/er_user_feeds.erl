-module(er_user_feeds).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {user_feeds=#{}}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    io:format("~p starting~n", [?MODULE]),
    register(?MODULE, self()),
    {ok, #state{}}.

handle_call({add, User, Uri}, _From, State = #state{user_feeds = UserFeeds}) ->
    io:format("Adding ~p to feeds for user ~p~n", [Uri, User]),
    {ok, UpdatedUserFeeds} = add_user_to_feed(User, Uri, UserFeeds),
    {reply, ok, State#state{user_feeds = UpdatedUserFeeds}};

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

add_user_to_feed(User, Uri, UserFeeds) ->
    UpdatedUserFeeds = case maps:is_key(User, UserFeeds) of
        true ->
            Feeds = maps:get(User, UserFeeds),
            UpdatedFeeds = [Uri | Feeds],
            maps:put(User, UpdatedFeeds, UserFeeds);
        false ->
            maps:put(User, [Uri], UserFeeds)
    end,
    {ok, UpdatedUserFeeds}.
