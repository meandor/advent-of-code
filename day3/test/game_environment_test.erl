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

should_move_to_position_test() ->
  GameState = #{
    agent_position => [1, 1],
    board_dimensions => [2, 3],
    board_layout => [
      [open_square, open_square],
      [open_square, tree],
      [open_square, open_square]
    ]
  },
  Actual = game_environment:move([1, 1], GameState),
  Expected = #{
    agent_position => [2, 2],
    board_dimensions => [2, 3],
    board_layout => [
      [open_square, open_square],
      [open_square, tree],
      [open_square, open_square]
    ]
  },

  ?assertEqual(Expected, Actual).

should_move_to_position_outside_of_right_border_test() ->
  GameState = #{
    agent_position => [2, 1],
    board_dimensions => [2, 3],
    board_layout => [
      [open_square, open_square],
      [tree, open_square],
      [open_square, open_square]
    ]
  },
  Actual = game_environment:move([1, 0], GameState),
  Expected = #{
    agent_position => [1, 1],
    board_dimensions => [2, 3],
    board_layout => [
      [open_square, open_square],
      [tree, open_square],
      [open_square, open_square]
    ]
  },

  ?assertEqual(Expected, Actual).

should_not_be_game_over_test() ->
  GameState = #{
    done => false,
    agent_position => [1, 1],
    board_dimensions => [2, 3]
  },
  Actual = game_environment:is_game_over(GameState),
  Expected = #{
    done => false,
    agent_position => [1, 1],
    board_dimensions => [2, 3]
  },

  ?assertEqual(Expected, Actual).

should_be_game_over_test() ->
  GameState = #{
    done => false,
    agent_position => [1, 3],
    board_dimensions => [2, 3]
  },
  Actual = game_environment:is_game_over(GameState),
  Expected = #{
    done => true,
    agent_position => [1, 3],
    board_dimensions => [2, 3]
  },

  ?assertEqual(Expected, Actual).
