-module(day3_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one,
    intensity => 2,
    period => 5},
  ChildSpecs = [
    #{id => game_environment,
      start => {game_environment, start_link, ["resources/input.txt"]},
      restart => permanent,
      shutdown => 5,
      type => worker}
  ],
  {ok, {SupFlags, ChildSpecs}}.
