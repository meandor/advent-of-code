# day3
An _Erlang OTP application_ build with [rebar3](https://github.com/erlang/rebar3) that calculates the number of trees a
Toboggan might encounter.

## Environment
> Due to the local geology, trees in this area only grow on exact integer coordinates in a grid. You make a map (your
> puzzle input) of the open squares (.) and trees (#) you can see. For example:
```
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
```

> These aren't the only trees, though; due to something you read about once involving arboreal genetics and biome
> stability, the same pattern repeats to the right many times:

```
..##.........##.........##.........##.........##.........##.......  --->
#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
.#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
.#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
.#........#.#........#.#........#.#........#.#........#.#........#
#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
#...##....##...##....##...##....##...##....##...##....##...##....#
.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
```

## Movement
> You start on the open square (.) in the top-left corner and need to reach the bottom (below the bottom-most row on
> your map).
>
> The toboggan can only follow a few specific slopes (you opted for a cheaper model that prefers rational numbers);
> start by counting all the trees you would encounter for the slope right 3, down 1:

## Build
```bash
rebar3 compile
```

## Run
```bash
rebar3 shell
```
And then invoke the game_agen with:
```erlang
gen_server:call(game_agent, {start, [MOVEMENT_VECTOR]}).
```
Where `MOVEMENT_VECTOR` is a vector like `[3,1]` which moves 3 right and 1 down.

E.g.:
```erlang
gen_server:call(game_agent, {start, [[1,1],[3,1],[5,1],[7,1],[1,2]]}).
```
