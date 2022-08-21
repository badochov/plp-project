
:- [probability, utils, problog_examples/program_3].

:- begin_tests(probability).

test(query_true):-
    prob(true, P),
    float_cmp(P, 1).

test(query_false):-
    prob(false, P),
    float_cmp(P, 0).

test(query_a_given_a):-
    prob(b(x1), b(x1), P),
    float_cmp(P, 1).

:- end_tests(probability).
