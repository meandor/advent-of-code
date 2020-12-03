#!/usr/bin/swipl -f -q

:- consult(day1).

:- initialization main.

day1_solution :-
    pairs_of_2020("resources/day1.txt", X, Y, Multiplied),
    write("Day 1.1 solution - Number 1: "),
    write(X),
    write(", Number 2: "),
    write(Y),
    write(", Multiplied: "),
    write(Multiplied),
    write("\n").

day1_1_solution :-
    pairs_of_2020("resources/day1.txt", X, Y, Z, Multiplied),
    write("Day 1.2 solution - Number 1: "),
    write(X),
    write(", Number 2: "),
    write(Y),
    write(", Number 3: "),
    write(Z),
    write(", Multiplied: "),
    write(Multiplied).

main :- day1_solution, day1_1_solution.
