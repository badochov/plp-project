% construct_bdd(+Formula, -BDD).
construct_bdd(F, BDD) :-
    get_variable_order(F, Vars),
    construct_obdd(F, Vars, OBDD),
    reduce_to_robdd(OBDD, BDD).

% construct_obdd(+Formula, +VariableOrdering, -OBDD)
construct_obdd(F, [], Leaf) :- 
    simplify_formula(F, Sf),
    to_leaf(Sf, Leaf).

to_leaf(false_, leaf(0)).
to_leaf(true_, leaf(1)).

is_literal(true_).
is_literal(false_).

get_probabiity(Var, P) :-
    % TODO 
    P = 1.

assign_to(and(F1, F2), V, A, and(Nf1, Nf2)) :-
    assign_to(F1, V, A, Nf1),
    assign_to(F2, V, A, Nf2).
assign_to(or(F1, F2), V, A, or(Nf1, Nf2)) :-
    assign_to(F1, V, A, Nf1),
    assign_to(F2, V, A, Nf2).
assign_to(not(F), V, A, not(Nf)) :-
    assign_to(F, V, A, Nf).
assign_to(V, V, A, A).
assign_to(F, _, _, F).

construct_obdd(F, [V|T], OBDD) :-
    simplify_formula(F, Sf),
    ( is_literal(Sf)
    ->
        to_leaf(Sf, OBDD)
    :
        get_probabiity(V, P),
        assign_to(F, V, false_, Ff),
        construct_obdd(Ff, T, FalseOBDD),
        assign_to(F, V, true_, Tf),
        construct_obdd(Tf, T, TrueOBDD),
        node(FalseOBDD, P, TrueOBDD)
    ).

% Simplifies formula removing falses and trues from the formula.
simplify_formula(and(true_, F), Sf) :- simplify_formula(F, Sf).
simplify_formula(and(F, true_), Sf) :- simplify_formula(F, Sf).
simplify_formula(and(false_, _), false_).
simplify_formula(and(_, false_), false_).
simplify_formula(and(F1, F2), Sf) :- 
    simplify_formula(F1, Sf1),
    simplify_formula(F2, Sf2),
    simplify_formula(and(Sf1, Sf2), Sf).
simplify_formula(or(false_, F), Sf) :- simplify_formula(F, Sf).
simplify_formula(or(F, false_), Sf) :- simplify_formula(F, Sf).
simplify_formula(or(true_, _), true_).
simplify_formula(or(_, true_), true_).
simplify_formula(or(F1, F2), Sf) :- 
    simplify_formula(F1, Sf1),
    simplify_formula(F2, Sf2),
    simplify_formula(or(Sf1, Sf2), Sf).
simplify_formula(not(false_), true_).
simplify_formula(not(true_), false_).
simplify_formula(not(F), Sf) :-
    simplify_formula(F, Sf1),
    simplify_formula(not(Sf1), Sf).
simplify_formula(F, F).

% reduce_to_robdd(+OBDD, -ROBDD)
reduce_to_robdd(OBDD, ROBDD) :- 
    % TODO
    OBDD = ROBDD.


% get_variable_order(+Formula, -FreeVars),
% Currently order is random.
get_variable_order(F, Vars) :- get_free_variables(F, Vars).

% get_free_variables(+Formula, -FreeVars),
get_free_variables(F, Vars) :- get_free_variables(F, [], Vars).
get_free_variables(not(F), Tmp, V) :- get_free_variables(F, Tmp, V).
get_free_variables(and(F1, F2), Tmp, V) :- 
    get_free_variables(F1, Tmp, Tmp1),
    get_free_variables(F2, Tmp1, V).
get_free_variables(or(F1, F2), Tmp, V) :- 
    get_free_variables(F1, Tmp, Tmp1),
    get_free_variables(F2, Tmp1, V).
get_free_variables(X, Tmp, [X|TMP]).