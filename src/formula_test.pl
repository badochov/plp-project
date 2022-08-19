
:- begin_tests(formula).

:- [grammar, ground, formula, problog_examples/program_3].

test(sorted_head):-
    findall(C, (C = (_ <--- _), C), P),
    ground(P,GP), 
    sorted_head(GP, L),
    L = [
        fr(x1)-[(a(x1, y1), \+b(x1)), (a(x1, y2), \+b(x1))],
        fr(x2)-[(a(x2, y1), \+b(x2)), (a(x2, y2), \+b(x2))]
    ].

test(formula):-
    findall(C, (C = (_ <--- _), C), P),
    ground(P,GP), 
    formula(GP, F),
    F = (
        ((\+fr(x1);a(x1, y1), \+b(x1);a(x1, y2), \+b(x1)),
            (\+ (a(x1, y1), \+b(x1);a(x1, y2), \+b(x1));fr(x1))),
        (\+fr(x2);a(x2, y1), \+b(x2);a(x2, y2), \+b(x2)),
        (\+ (a(x2, y1), \+b(x2);a(x2, y2), \+b(x2));fr(x2))
        ). 

:- end_tests(formula).