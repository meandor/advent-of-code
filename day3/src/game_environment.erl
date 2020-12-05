-module(game_environment).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([parse_layout/1, move/2, is_game_over/1]).

-define(SERVER, ?MODULE).
-define(OPEN_SQUARE, open_square).
-define(TREE, tree).

start_link(LayoutFile) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [LayoutFile], []).

init([LayoutFile]) ->
  io:fwrite("Start loading game environment~n"),
  io:fwrite("Start loading game environment layout file: ~p~n", [LayoutFile]),
  {ok, Layout} = file:read_file(LayoutFile),
  ParsedLayout = parse_layout(Layout),
  [FirstRow | _Rest] = ParsedLayout,
  io:fwrite("Done loading game environment layout file~n"),
  InitialState = #{
    done => false,
    agent_position => [0, 0],
    board_dimensions => [length(FirstRow), length(ParsedLayout)],
    board_layout => ParsedLayout
  },
  io:fwrite("Done loading game environment~n"),
  {ok, InitialState}.

handle_call({step, Action}, _From, GameState) ->
  MovedState = move(Action, GameState),
  CheckedDoneState = is_game_over(MovedState),
  Observation = board_at(maps:get(agent_position, CheckedDoneState), CheckedDoneState),
  {reply, {Observation, maps:get(done, CheckedDoneState)}, CheckedDoneState}.

handle_cast(_Request, _State) ->
  erlang:error(not_implemented).

board_at([X, Y], GameState) ->
  Layout = maps:get(board_layout, GameState),
  Row = lists:nth(Y + 1, Layout),
  lists:nth(X + 1, Row).

is_game_over(GameState) ->
  [_AgentXPosition, AgentYPosition] = maps:get(agent_position, GameState),
  [_SizeX, SizeY] = maps:get(board_dimensions, GameState),
  maps:put(done, (SizeY - 1) == AgentYPosition, GameState).

move([MoveX, MoveY], GameState) ->
  [CurrentX, CurrentY] = maps:get(agent_position, GameState),
  [SizeX, _SizeY] = maps:get(board_dimensions, GameState),
  NewPosition = [(CurrentX + MoveX) rem SizeX, CurrentY + MoveY],
  maps:put(agent_position, NewPosition, GameState).

to_field(<<".">>) -> ?OPEN_SQUARE;
to_field(<<"#">>) -> ?TREE.

parse_layout_line(LayoutLine) ->
  Characters = re:split(LayoutLine, "(?!^)"),
  [_H | T] = lists:reverse(Characters),
  SanitizedCharacters = lists:reverse(T),
  lists:map(fun to_field/1, SanitizedCharacters).

parse_layout(Layout) ->
  LayoutLines = re:split(Layout, "\n"),
  LayoutMatrix = lists:map(fun parse_layout_line/1, LayoutLines),
  lists:filter(fun(X) -> X =/= [] end, LayoutMatrix).
