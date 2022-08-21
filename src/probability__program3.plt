
:- begin_tests(probability__program_3).

:- [probability, utils, problog_examples/program_3].

test(query_true):-
    prob(true, P),
    float_cmp(P, 1).

test(query_false):-
    prob(false, P),
    float_cmp(P, 0).

test(query_a_given_a):-
    prob(b(x1), b(x1), P),
    float_cmp(P, 1).

test(query_b_x1):-
    prob(b(x1), P),
    float_cmp(P, 0.3).

% Probability tested with cplint
test(query_fr_X):-
    prob(fr(X), P),
    float_cmp(P, 0.83).

test(query_fr_X_b_X):-
    % Never possible.
    prob((fr(X),b(X)), P),
    float_cmp(P, 0).

test(query_b_X):-
    prob(b(X), P),
    float_cmp(P, 0.51).

:- end_tests(probability__program_3).
