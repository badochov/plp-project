:- [grammar, program_3].

:- begin_tests(grammar).

test(problog_collect):-
    problog_collect(P), length(P, 4).

:- end_tests(grammar).