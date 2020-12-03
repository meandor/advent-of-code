%%%-------------------------------------------------------------------
%% @doc day3 public API
%% @end
%%%-------------------------------------------------------------------

-module(day3_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    day3_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
