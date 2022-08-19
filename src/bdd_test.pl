:- begin_tests(bdd).
:- [bdd].

test(construct_bdd__not) :- 
    rb_empty(Probs),
    rb_insert(Probs, x, 0.42, Ps),
    construct_bdd(not(x), Ps, node(leaf(1), 0.42, leaf(0))).

% Run obdd instead of bdd to ensure order.
test(construct_obdd__simple) :- 
    rb_empty(Probs),
    rb_insert(Probs, x, 0.42, Px),
    rb_insert(Px, y, 0.6, Py),
    rb_insert(Py, z, 0.9, Pz),
    construct_obdd(or(not(and(x, y)), z), [z, y, x], Pz, node(node(leaf(1),0.6,node(leaf(1),0.42,leaf(0))),0.9,leaf(1))).

test(reduce_to_robdd__leafs) :-
    reduce_to_robdd(node(leaf(0), 1, leaf(0)), leaf(0)).

test(reduce_to_robdd__nodes) :-
    reduce_to_robdd(node(node(leaf(0), 1, leaf(1)), 1, node(leaf(0), 1, leaf(1))), node(leaf(0), 1, leaf(1))).

test(reduce_to_robdd__mixed) :-
    reduce_to_robdd(node(leaf(0), 1, node(leaf(0), 1, leaf(0))), leaf(0)).

:- end_tests(bdd).