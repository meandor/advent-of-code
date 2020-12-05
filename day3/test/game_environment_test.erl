-module(game_environment_test).

-include_lib("eunit/include/eunit.hrl").

should_parse_single_line_test() ->
  Actual = game_environment:parse_layout(<<".#">>),
  Expected = [[open_square, tree]],

  ?assertEqual(Expected, Actual).

should_parse_multiple_lines_test() ->
  Actual = game_environment:parse_layout(<<".#.\n...\n#.#\n">>),
  Expected = [
    [open_square, tree, open_square],
    [open_square, open_square, open_square],
    [tree, open_square, tree]
  ],

  ?assertEqual(Expected, Actual).
