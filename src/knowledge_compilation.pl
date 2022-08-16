:-[grounding].
:-[to_logic_formula].
:-[break_cycles].
:-[bdd].
:-[calc_probability].

% to_bdd(+Clause, -BDD)
to_bdd(Clause, BDD) :-
    ground_program,
    break_cycles,
    to_logic_formula(Clause, F),
    construct_bdd(F, BDD).

% calculate_probability(+Clause, -BDD)
calculate_probability(Clause, P) :-
    to_bdd(Clause, BDD),
    calc_probability(BDD, P).