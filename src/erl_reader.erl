-module(erl_reader).
-behaviour(gen_server).

-export([start/0, start_link/0, add_feed/1, list_feeds/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erl_reader.hrl").
-include("deps/atomizer/src/atomizer.hrl").

-record(state, {feeds=[]}).

start() ->
    application:start(inets),
    application:start(?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    {ok, #state{}}.

handle_call({add, Uri}, _From, State = #state{feeds=F}) ->
    try atomizer:parse_url(Uri) of
        unknown ->
            {reply, bad_feed, State};
        Feed ->
            {reply, ok, #state{feeds=[create_feed(Feed)|F]}}
    catch
        throw:Reason -> {reply, Reason, State}
    end;

handle_call(list, _From, State) ->
    Reply = lists:map(fun(F) -> F#er_feed.uri end, State#state.feeds),
    {reply, Reply, State};

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

add_feed(Uri) ->
    gen_server:call(?MODULE, {add, Uri}).

list_feeds() ->
    gen_server:call(?MODULE, list).

create_feed(Feed) ->
    #er_feed{uri=Feed#feed.url}.

