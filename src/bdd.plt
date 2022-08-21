:- begin_tests(bdd).
:- [bdd].

test(construct_bdd__not) :- 
    construct_bdd(not(x), node(leaf(1), x, leaf(0))).

% Run obdd instead of bdd to ensure order.
test(construct_obdd__simple) :-
    construct_obdd(or(not(and(x, y)), z), [z, y, x],
        node(node(leaf(1),y,node(leaf(1),x,leaf(0))),z,leaf(1))).

test(reduce_to_robdd__leafs) :-
    reduce_to_robdd(node(leaf(0), 1, leaf(0)), leaf(0)).

test(reduce_to_robdd__nodes) :-
    reduce_to_robdd(node(node(leaf(0), 1, leaf(1)), 1, node(leaf(0), 1, leaf(1))), node(leaf(0), 1, leaf(1))).

test(reduce_to_robdd__mixed) :-
    reduce_to_robdd(node(leaf(0), 1, node(leaf(0), 1, leaf(0))), leaf(0)).

:- end_tests(bdd).