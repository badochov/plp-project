
:- [calc_probability, bdd].

% Main probability query method
% prob(+Query, ?Evidence, -Probability)
prob(Query, Evidence, Prob):-
    problog_collect(P),
    ground_program(P, GLs, GP),
    program_formula(GP, Pf), formula(Query, Qf), formula(Evidence, Ef),
    reduce_and_product([Pf, Qf, Ef], F1),
    simplify_formula(F1, F1s), construct_bdd(F1s, BDD1),

    reduce_and_product([Pf, Ef], F2),
    simplify_formula(F2, F2s), construct_bdd(F2s, BDD2),

    grounded_literals_to_variable_probabilities_tree(GLs, VarPs),
    calc_probability(BDD1, VarPs, ProbQ),
    calc_probability(BDD2, VarPs, ProbE),
    Prob is ProbQ / ProbE, !.

prob(Query, Prob):-
    problog_collect(P),
    ground_program(P, GLs, GP),
    program_formula(GP, Pf), formula(Query, Qf),
    reduce_and_product([Pf, Qf], F1),
    simplify_formula(F1, F1s), construct_bdd(F1s, BDD1),

    grounded_literals_to_variable_probabilities_tree(GLs, VarPs),
    calc_probability(BDD1, VarPs, Prob), !.