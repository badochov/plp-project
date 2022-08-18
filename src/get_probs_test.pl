:- begin_tests(get_probs).

:- use_module(library(rbtrees)).
:- [grammar, get_probs, 'test_assets/get_prob'].


cmp_rb_trees(T1, T2) :-
    rb_size(T1, Size),
    rb_size(T2, Size),
    rb_keys(T1, Keys),
    cmp_rb_trees_helper(Keys, T1, T2).
    
cmp_rb_trees_helper([H|T], T1, T2) :-
     rb_lookup(H, Value, T1),
     rb_lookup(H, Value, T2),
     cmp_rb_trees_helper(T, T1, T2).
cmp_rb_trees_helper([], _, _).
    

test(get_probs__simple) :-
    get_prob_clauses(ProbClauses),
    get_probs(ProbClauses, Probs),
    list_to_rbtree([(b-0.6)], Tree),
    cmp_rb_trees(Tree, Probs).

:- end_tests(get_probs).
