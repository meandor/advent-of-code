#!/usr/bin/swipl -f -q

:- consult(day1).

:- initialization main.

day1_solution :-
    pairs_of_2020("resources/day1.txt", X, Y, Z),
    write("Day 1 solution - Number 1: "),
    write(X),
    write(", Number 2: "),
    write(Y),
    write(", Multiplied: "),
    write(Z).

main :- day1_solution.
