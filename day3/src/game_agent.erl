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
  TreeCount = play_and_count_trees([3, 1]),
  {reply, string:concat("Trees in path: ", integer_to_list(TreeCount)), State};
handle_call({start, Movements}, _From, State) ->
  TreeCounts = lists:map(fun play_and_count_trees/1, Movements),
  TotalTreeCount = lists:foldl(fun(X, Sum) -> X * Sum end, 1, TreeCounts),
  {reply, string:concat("Trees in path: ", integer_to_list(TotalTreeCount)), State}.

handle_cast(_Request, _State) ->
  erlang:error(not_implemented).

play_and_count_trees(MovementVector) ->
  io:fwrite("Starting game~n"),
  Path = play(MovementVector, false, []),
  Trees = lists:filter(fun(X) -> X == tree end, Path),
  TreeCount = length(Trees),
  io:fwrite("Found trees: ~p~n", [integer_to_list(TreeCount)]),
  TreeCount.

play(_MovementVector, true, AccPath) ->
  io:fwrite("Agent done~n"),
  ok = gen_server:call(game_environment, {reset}),
  AccPath;
play(MovementVector, false, AccPath) ->
  {Observation, Done} = gen_server:call(game_environment, {step, MovementVector}),
  play(MovementVector, Done, [Observation | AccPath]).
