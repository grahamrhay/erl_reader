-module(er_feed_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{er_feed, {er_feed, start_link, []},
            temporary, brutal_kill, worker, [er_feed]}]}}.
