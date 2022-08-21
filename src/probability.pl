
:- [calc_probability, bdd].

% Main probability query method
% prob(+Query, ?Evidence, -Probability)
prob(Query, Evidence, Prob):-
    problog_collect(P),
    ground_program(P, GLs, GP),
    program_formula(GP, Pf), formula(Query, Qf), formula(Evidence, Ef),
    reduce_and_product([Pf, Qf, Ef], F),
    simplify_formula(F, Fs),
    construct_bdd(Fs, BDD),
    grounded_literals_to_variable_probabilities_tree(GLs, VarPs),
    calc_probability(BDD, VarPs, Prob), !.

prob(Query, Prob):-
    prob(Query, true, Prob).