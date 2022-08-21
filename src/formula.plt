
:- begin_tests(formula).

:- [grounding, formula, problog_examples/program_3].

test(sorted_head):-
    problog_collect(P),
    ground_program(P,GL, GP),
    sorted_head(GP, L),
    L = [
        fr(x1)-[(a(x1,y1),\+b(x1)),(a(x1,y2),\+b(x1))],
        fr(x2)-[(a(x2,y1),\+b(x2)),(a(x2,y2),\+b(x2))],

        prob_fr(x1)-[(prob_fr_err_1(x1,y1),a(x1,y1),\+b(x1)),(prob_fr_err_1(x1,y2),a(x1,y2),\+b(x1))],
        prob_fr(x2)-[(prob_fr_err_1(x2,y1),a(x2,y1),\+b(x2)),(prob_fr_err_1(x2,y2),a(x2,y2),\+b(x2))]
    ].

test(rewrite_to_Hubert_syntax):-
    B = (a(x1,y1),\+b(x1)),
    rewrite_to_Hubert_syntax(B, Bh),
    Bh = and(a(x1,y1),not(b(x1))).

test(program_formula):-
    problog_collect(P),
    ground_program(P, GL, GP),
    program_formula(GP, F),
    GL = [
        ::(0.3,b(x1)),
        ::(0.3,b(x2)),

        ::(0.6,a(x1,y1)),
        ::(0.6,a(x1,y2)),
        ::(0.6,a(x2,y1)),
        ::(0.6,a(x2,y2)),

        ::(0.8,prob_fr_err_1(x1,y1)),
        ::(0.8,prob_fr_err_1(x1,y2)),
        ::(0.8,prob_fr_err_1(x2,y1)),
        ::(0.8,prob_fr_err_1(x2,y2))],
    F = and(and(or(not(fr(x1)),or(and(a(x1,y1),not(b(x1))),and(a(x1,y2),not(b(x1))))),or(not(or(and(a(x1,y1),not(b(x1))),and(a(x1,y2),not(b(x1))))),fr(x1))),and(and(or(not(fr(x2)),or(and(a(x2,y1),not(b(x2))),and(a(x2,y2),not(b(x2))))),or(not(or(and(a(x2,y1),not(b(x2))),and(a(x2,y2),not(b(x2))))),fr(x2))),and(and(or(not(prob_fr(x1)),or(and(prob_fr_err_1(x1,y1),and(a(x1,y1),not(b(x1)))),and(prob_fr_err_1(x1,y2),and(a(x1,y2),not(b(x1)))))),or(not(or(and(prob_fr_err_1(x1,y1),and(a(x1,y1),not(b(x1)))),and(prob_fr_err_1(x1,y2),and(a(x1,y2),not(b(x1)))))),prob_fr(x1))),and(or(not(prob_fr(x2)),or(and(prob_fr_err_1(x2,y1),and(a(x2,y1),not(b(x2)))),and(prob_fr_err_1(x2,y2),and(a(x2,y2),not(b(x2)))))),or(not(or(and(prob_fr_err_1(x2,y1),and(a(x2,y1),not(b(x2)))),and(prob_fr_err_1(x2,y2),and(a(x2,y2),not(b(x2)))))),prob_fr(x2))))))
    .

test(formula_given_evidence):-
    problog_collect(P),
    ground_program(P, _, GP),
    program_formula(GP, F1),
    formula_given_evidence(F1, and(b(x1), not(b(x2))), F2),
    F2 = and(not(fr(x1)),and(and(or(not(fr(x2)),or(a(x2,y1),a(x2,y2))),or(not(or(a(x2,y1),a(x2,y2))),fr(x2))),and(not(prob_fr(x1)),and(or(not(prob_fr(x2)),or(and(prob_fr_err_1(x2,y1),a(x2,y1)),and(prob_fr_err_1(x2,y2),a(x2,y2)))),or(not(or(and(prob_fr_err_1(x2,y1),a(x2,y1)),and(prob_fr_err_1(x2,y2),a(x2,y2)))),prob_fr(x2)))))).

:- end_tests(formula).