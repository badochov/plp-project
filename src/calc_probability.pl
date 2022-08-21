:- dynamic cached_probability/2.

:- [grammar].

:- use_module(library(rbtrees)).

% calc_probability(+BDD, +VariableProbabilities, -Probability)
% Calculates probability for given BDD.
% VariableProbabilities is an RB tree of (Var -> [ProbYes,ProbNo]
grounded_literals_to_variable_probabilities_tree([], VarPsRB):-
    rb_new(VarPsRB).

grounded_literals_to_variable_probabilities_tree([GL|GLs], VarPsRB):-
    grounded_literals_to_variable_probabilities_tree(GLs, VarPsRB1),
    (P :: Head) = GL,
    rb_insert(VarPsRB1, Head, P, VarPsRB).

get_var_probability(Var, VarPsRB, ProbYes, ProbNo):-
        rb_lookup(Var, ProbYes, VarPsRB), ProbNo is 1 - ProbYes, !.
get_var_probability(Var, VarPsRB, 1, 1).


calc_probability(BDD, VarProbabilities, P) :-
    clean_cache,
    calc_probability_(BDD, VarProbabilities, P),
    clean_cache.

calc_probability_(leaf(P), _, P) :- !.
calc_probability_(N, _, P) :- get_from_cache(N, P), !.
calc_probability_(node(LOBDD, Var, ROBDD), VarPs, P) :-
        calc_probability_(LOBDD, VarPs, Lp),
        calc_probability_(ROBDD, VarPs, Rp),
        get_var_probability(Var, VarPs, ProbYes, ProbNo),
        P is Rp * ProbYes + Lp * ProbNo.
        save_to_cache(node(LOBDD, Np, ROBDD), P).

get_from_cache(N, P) :- cached_probability(N, P).
save_to_cache(N, P) :- asserta(cached_probability(N, P)).
clean_cache:- retractall(cached_probability(_, _)).

    
