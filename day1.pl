read_input(File, Input) :-
    read_file_to_string(File, RawInput, []),
    split_string(RawInput, "\n", "\n", Input).

sum_is_2020(A, B) :-
    number_string(ANumber, A),
    number_string(BNumber, B),
    (2020 is (ANumber + BNumber)).

find_2020_pairs(Values, X, Y) :-
    member(X, Values),
    member(Y, Values),
    sum_is_2020(X, Y).

pairs_of_2020(File, XNumber, YNumber, Z) :-
    read_input(File, Input),
    find_2020_pairs(Input, X, Y),
    number_string(XNumber, X),
    number_string(YNumber, Y),
    (Z is (XNumber * YNumber)).
