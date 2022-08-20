:- [formula].

% construct_bdd(+Formula, -BDD).
construct_bdd(F, BDD) :-
    get_variable_order(F, Vars),
    construct_obdd(F, Vars, OBDD),
    reduce_to_robdd(OBDD, BDD).

to_leaf(false_, leaf(0)).
to_leaf(true_, leaf(1)).

is_literal(true_).
is_literal(false_).

get_probability(Var, Ps, P) :- rb_lookup(Var, P, Ps).

assign_to(and(F1, F2), V, A, and(Nf1, Nf2)) :-
    assign_to(F1, V, A, Nf1),
    assign_to(F2, V, A, Nf2), !.
assign_to(or(F1, F2), V, A, or(Nf1, Nf2)) :-
    assign_to(F1, V, A, Nf1),
    assign_to(F2, V, A, Nf2), !.
assign_to(not(F), V, A, not(Nf)) :-
    assign_to(F, V, A, Nf), !.
assign_to(V, V, A, A) :- !.
assign_to(F, _, _, F) :- !.

% construct_obdd(+Formula, +VariableOrdering, +VariableProbabilities, -OBDD)
construct_obdd(F, [], Leaf) :- 
    simplify_formula(F, Sf),
    to_leaf(Sf, Leaf).
construct_obdd(Formula, [V|T], OBDD) :-
    simplify_formula(Formula, Fs),
    ( is_literal(Fs)
    ->
        to_leaf(Fs, OBDD)
    ;(
        has_variable(Fs, V) -> (
            assign_to(Fs, V, false_, Ff),
            construct_obdd(Ff, T, FalseOBDD),
            assign_to(Fs, V, true_, Tf),
            construct_obdd(Tf, T, TrueOBDD),
            OBDD = node(FalseOBDD, V, TrueOBDD)
        ); (
            construct_obdd(Fs, T, OBDD)
        )
    )), !.

% Simplifies formula removing falses and trues from the formula.
simplify_formula(F, Sf) :- simplify_formula_const(F, Sf), !.
simplify_formula(and(F1, F2), Sf) :- 
    simplify_formula(F1, Sf1),
    simplify_formula(F2, Sf2),
    simplify_formula_const(and(Sf1, Sf2), Sf), !.
simplify_formula(or(F1, F2), Sf) :- 
    simplify_formula(F1, Sf1),
    simplify_formula(F2, Sf2),
    simplify_formula_const(or(Sf1, Sf2), Sf), !.
simplify_formula(not(F), Sf) :-
    simplify_formula(F, Sf1),
    simplify_formula_const(not(Sf1), Sf), !.
simplify_formula(F, F) :- !.

simplify_formula_const(and(true_, F), Sf) :- simplify_formula(F, Sf), !.
simplify_formula_const(and(F, true_), Sf) :- simplify_formula(F, Sf), !.
simplify_formula_const(and(false_, _), false_) :- !.
simplify_formula_const(and(_, false_), false_) :- !.
simplify_formula_const(or(false_, F), Sf) :- simplify_formula(F, Sf), !.
simplify_formula_const(or(F, false_), Sf) :- simplify_formula(F, Sf), !.
simplify_formula_const(or(true_, _), true_) :- !.
simplify_formula_const(or(_, true_), true_) :- !.
simplify_formula_const(not(false_), true_) :- !.
simplify_formula_const(not(true_), false_) :- !.

% reduce_to_robdd(+OBDD, -ROBDD)
reduce_to_robdd(OBDD, ROBDD) :- 
    apply_elimination_rule(OBDD, Eliminated),
    apply_isomorphism_rule(Eliminated, Isomorphed),
    ( OBDD = Isomorphed
    ->
        OBDD = ROBDD
    ;
        reduce_to_robdd(Isomorphed, ROBDD)
    ).


apply_elimination_rule(leaf(X), leaf(X)).
apply_elimination_rule(node(LOBDD, Val, ROBDD), Eliminated) :-
    apply_elimination_rule(LOBDD, ElL),
    apply_elimination_rule(ROBDD, ElR),
    ( ElL = ElR
    ->
        Eliminated = ElR
    ;
        Eliminated = node(ElL, Val, ElR)
    ).


apply_isomorphism_rule(OBDD, Isomorphed) :-
    % Isomorphism is implicit in prolog, that is probability for same node will be calculated only once in calc_probability even without isomorphic elimiation.
    % Probably structure would be more efficient ans isomorphism made sense if nodes where *pointers* to nodes instead of nodes.
    OBDD = Isomorphed.


% get_variable_order(+Formula, -FreeVars),
% Currently order is random.
get_variable_order(F, Vars) :- get_free_variables(F, Vars).

% get_free_variables(+Formula, -FreeVars),
get_free_variables(F, Vars) :- get_free_variables(F, [], Vars), !.
get_free_variables(not(F), Tmp, V) :- get_free_variables(F, Tmp, V), !.
get_free_variables(and(F1, F2), Tmp, V) :- 
    get_free_variables(F1, Tmp, Tmp1),
    get_free_variables(F2, Tmp1, V), !.
get_free_variables(or(F1, F2), Tmp, V) :- 
    get_free_variables(F1, Tmp, Tmp1),
    get_free_variables(F2, Tmp1, V), !.
get_free_variables(X, Tmp, [X|Tmp]) :- !.


has_variable(F, Var):-
    get_free_variables(F, Vars), member(Var, Vars), !.