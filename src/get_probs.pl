:- use_module(library(rbtrees)).
:- [grammar].
:- [grounding].


get_prob_clauses(Clauses) :-
    findall(C, (C = (_ :: _), C), Clauses).

get_probs(Clauses, Probs) :-
    rb_empty(Tree),
    ground(Clauses, GroundedClauses),
    get_probs(GroundedClauses, Tree, Probs).
    
get_probs([(P::H)|T], Tmp, Probs) :-
    rb_insert(Tmp, H, P, Tree),
    get_probs(T, Tree, Probs).
get_probs([], Probs, Probs).