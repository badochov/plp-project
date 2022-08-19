
:- begin_tests(formula).

:- [grounding, formula, problog_examples/program_3].

test(sorted_head):-
    problog_collect(P),
    ground(P,GL, GP),
    sorted_head(GP, L),
    L = [
        fr(x1)-[(a(x1,y1),\+b(x1)),(a(x1,y2),\+b(x1))],
        fr(x2)-[(a(x2,y1),\+b(x2)),(a(x2,y2),\+b(x2))],

        prob_fr(x1)-[(a(x1,y1),\+b(x1)),(a(x1,y2),\+b(x1))],
        prob_fr(x2)-[(a(x2,y1),\+b(x2)),(a(x2,y2),\+b(x2))]
        ].

test(rewrite_to_Hubert_syntax):-
    B = (a(x1,y1),\+b(x1)),
    rewrite_to_Hubert_syntax(B, Bh),
    Bh = and(a(x1,y1),not(b(x1))).

test(formula):-
    problog_collect(P),
    ground(P,GL, GP),
    formula(GP, F),
    F = and(and(or(not(fr(x1)),or(and(a(x1,y1),not(b(x1))),and(a(x1,y2),not(b(x1))))),or(not(or(and(a(x1,y1),not(b(x1))),and(a(x1,y2),not(b(x1))))),fr(x1))),and(and(or(not(fr(x2)),or(and(a(x2,y1),not(b(x2))),and(a(x2,y2),not(b(x2))))),or(not(or(and(a(x2,y1),not(b(x2))),and(a(x2,y2),not(b(x2))))),fr(x2))),and(and(or(not(prob_fr(x1)),or(and(a(x1,y1),not(b(x1))),and(a(x1,y2),not(b(x1))))),or(not(or(and(a(x1,y1),not(b(x1))),and(a(x1,y2),not(b(x1))))),prob_fr(x1))),and(or(not(prob_fr(x2)),or(and(a(x2,y1),not(b(x2))),and(a(x2,y2),not(b(x2))))),or(not(or(and(a(x2,y1),not(b(x2))),and(a(x2,y2),not(b(x2))))),prob_fr(x2)))))).

:- end_tests(formula).