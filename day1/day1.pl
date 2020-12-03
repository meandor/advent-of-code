read_input(File, Input) :-
    read_file_to_string(File, RawInput, []),
    split_string(RawInput, "\n", "\n", Input).

sum_is_2020(A, B, C) :-
    number_string(ANumber, A),
    number_string(BNumber, B),
    number_string(CNumber, C),
    (2020 is (ANumber + BNumber + CNumber)).

find_2020_pairs(Values, X, Y) :-
    member(X, Values),
    member(Y, Values),
    sum_is_2020(X, Y, "0").

find_2020_triplet(Values, X, Y, Z) :-
    member(X, Values),
    member(Y, Values),
    member(Z, Values),
    sum_is_2020(X, Y, Z).

pairs_of_2020(File, XNumber, YNumber, Multiplication) :-
    read_input(File, Input),
    find_2020_pairs(Input, X, Y),
    number_string(XNumber, X),
    number_string(YNumber, Y),
    (Multiplication is (XNumber * YNumber)).

pairs_of_2020(File, XNumber, YNumber, ZNumber, Multiplication) :-
    read_input(File, Input),
    find_2020_triplet(Input, X, Y, Z),
    number_string(XNumber, X),
    number_string(YNumber, Y),
    number_string(ZNumber, Z),
    (Multiplication is (XNumber * YNumber * ZNumber)).
