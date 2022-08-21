
:- [probability, utils, problog_examples/program_3].

:- begin_tests(probability).

test(query_true):-
    prob(true, P),
    float_cmp(P, 1).


:- end_tests(probability).
