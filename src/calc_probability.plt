:- begin_tests(calc_probability).

:- [calc_probability, utils].

test(calc_probability__1) :- 
    list_to_rbtree([x-0.42], VarPs),
    calc_probability(node(leaf(1), x, leaf(0)), VarPs, P),
    float_cmp(P, 0.58).

test(calc_probability__2) :- 
    list_to_rbtree([x-0.42], VarPs),
    calc_probability(node(leaf(0), x, leaf(1)), VarPs, P),
    float_cmp(P, 0.42).

test(calc_probability__3) :- 
    list_to_rbtree([x-0.42, y-0.6, z-0.9], VarPs),
    calc_probability(
        node(node(leaf(1),x,node(leaf(1),y,leaf(0))),z,leaf(1)),
        VarPs, P),
    float_cmp(P, 0.9748).

:- end_tests(calc_probability).