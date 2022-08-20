:- dynamic cached_probability/2.

:- use_module(library(rbtrees)).


grounded_literals_to_variable_probabilities_tree([], VarPsRB):-
    rb_new(VarPsRB).

grounded_literals_to_variable_probabilities_tree([GL|GLs], VarPsRB):-
    grounded_literals_to_variable_probabilities_tree(GLs, VarPsRB1),
    (P :: Head) = GL,
    rb_insert(VarPsRB1, Head, P, VarPsRB).



% calc_probability(+BDD, +VariableProbabilities, -Probability)
% Calculates probability for given BDD.
% VariableProbabilities is an RB tree of (Var -> [ProbYes,ProbNo]

grounded_literals_to_variable_probabilities_tree([], VarPsRB):-
    rb_new(VarPsRB).

grounded_literals_to_variable_probabilities_tree([GL|GLs], VarPsRB):-
    grounded_literals_to_variable_probabilities_tree(GLs, VarPsRB1),
    (P :: Head) = GL,
    rb_insert(VarPsRB1, Head, P, VarPsRB).
)
calc_probability(BDD, VarProbabilities, P) :-
    clean_cache,
    calc_probability_(BDD, VarProbabilities, P),
    clean_cache.

calc_probability_(leaf(P), _, P) :- !.
calc_probability_(N, _, P) :- get_from_cache(N, P), !.
calc_probability_(node(LOBDD, Var, ROBDD), VarPs, P) :-
        calc_probability_(LOBDD, VarPs, Lp),
        calc_probability_(ROBDD, VarPs, Rp),
        rb_lookup(Var, Np, VarPs),
        P is Rp * Np + Lp * (1 - Np),
        save_to_cache(node(LOBDD, Np, ROBDD), P).

get_from_cache(N, P) :- cached_probability(N, P).
save_to_cache(N, P) :- asserta(cached_probability(N, P)).
clean_cache:- retractall(cached_probability(_, _)).

    
