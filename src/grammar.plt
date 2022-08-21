:- [grammar, problog_examples/program_3].

:- begin_tests(grammar).

test(collect_vars):-
    collect_vars(fr(X), (a(X,Y),\+b(X)),Vars),
    Vars = [X, Y].

test(problog_collect):-
    problog_collect(P), length(P, 5).

:- end_tests(grammar).