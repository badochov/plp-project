:- begin_tests(calc_probability).

:- [calc_probability].

float_cmp(F1, F2) :- float_cmp(F1, F2, 0.001).
float_cmp(F1, F2, Eps) :- F1 - Eps =< F2, F1 + Eps >= F2.

test(calc_probability__1) :- 
    rb_new(VarPs1), rb_insert(VarPs1, x, 0.42, VarPs),
    calc_probability(node(leaf(1), x, leaf(0)), VarPs, P),
    float_cmp(P, 0.58).

test(calc_probability__2) :- 
    rb_new(VarPs1), rb_insert(VarPs1, x, 0.42, VarPs),
    calc_probability(node(leaf(0), x, leaf(1)), VarPs, P),
    float_cmp(P, 0.42).

test(calc_probability__3) :- 
    rb_new(VarPs1),
    rb_insert(VarPs1, x, 0.42, VarPs2),
    rb_insert(VarPs2, y, 0.6, VarPs3),
    rb_insert(VarPs3, z, 0.9, VarPs),
    calc_probability(
        node(node(leaf(1),x,node(leaf(1),y,leaf(0))),z,leaf(1)),
        VarPs, P),
    float_cmp(P, 0.9748).

:- end_tests(calc_probability).