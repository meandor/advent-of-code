-module(game_agent).

-behaviour(gen_server).

-export([start_link/0, handle_cast/2]).
-export([init/1, handle_call/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [[]], []).

init(State) ->
  {ok, State}.

handle_call(start, _From, State) ->
  Path = play(false, []),
  Trees = lists:filter(fun(X) -> X == tree end, Path),
  {reply, string:concat("Trees in path: ", integer_to_list(length(Trees))), State}.

handle_cast(_Request, _State) ->
  erlang:error(not_implemented).

play(true, AccPath) ->
  io:fwrite("Agent done~n"),
  AccPath;
play(false, AccPath) ->
  io:fwrite("Making one move~n"),
  {Observation, Done} = gen_server:call(game_environment, {step, [3, 1]}),
  io:fwrite("Got field: ~p~n", [Observation]),
  play(Done, [Observation | AccPath]).
