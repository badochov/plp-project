:-[bdd].

% Currently probability for a variable is always 42.
test__construct_bdd__not :- construct_bdd(not(x), node(leaf(1), 0.42, leaf(0))).
test__construct_bdd__simple :- construct_bdd(or(not(and(x, y)), z), node(node(leaf(1),0.42,node(leaf(1),0.42,leaf(0))),0.42,leaf(1))).

test__construct_bdd :-
    test__construct_bdd__not,
    test__construct_bdd__simple.