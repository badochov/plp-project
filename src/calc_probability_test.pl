:- [calc_probability].

float_cmp(F1, F2) :- float_cmp(F1, F2, 0.001).
float_cmp(F1, F2, Eps) :- F1 - Eps =< F2, F1 + Eps >= F2.

test__calc_probability__1 :- 
    calc_probability(node(leaf(1), 0.42, leaf(0)), P),
    float_cmp(P, 0.58).

test__calc_probability__2 :- 
    calc_probability(node(leaf(0), 0.42, leaf(1)), P),
    float_cmp(P, 0.42).

test__calc_probability__3 :- 
    calc_probability(node(node(leaf(1),0.42,node(leaf(1),0.42,leaf(0))),0.42,leaf(1)), P),
    float_cmp(P, 0.897688).


test__calc_probability :-
    test__calc_probability__1,
    test__calc_probability__2,
    test__calc_probability__3.
    
