:- [formula].

% construct_bdd(+Formula, -BDD).
construct_bdd(F, BDD) :-
    get_variable_order(F, Vars),
    construct_obdd(F, Vars, OBDD),
    reduce_to_robdd(OBDD, BDD).

to_leaf(false, leaf(0)).
to_leaf(true, leaf(1)).

is_literal(true).
is_literal(false).

get_probability(Var, Ps, P) :- rb_lookup(Var, P, Ps).

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
            assign_to(Fs, V, false, Ff),
            construct_obdd(Ff, T, FalseOBDD),
            assign_to(Fs, V, true, Tf),
            construct_obdd(Tf, T, TrueOBDD),
            OBDD = node(FalseOBDD, V, TrueOBDD)
        ); (
            construct_obdd(Fs, T, OBDD)
        )
    )), !.

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