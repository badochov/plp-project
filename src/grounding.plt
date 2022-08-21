
:- begin_tests(ground).

is_permutation(Xs, Ys) :-
  msort(Xs, Sorted),
  msort(Ys, Sorted).

:- [grounding, problog_examples/program_3].

test(function_sort_bind_variables_zero_var):-
    findall(Picked, 
        function_sort_bind_variables(V-V, _, [x1, y2], [[x1,x2], [y1,y2]], Picked),
        AllPicks), AllPicks = [[x1,y2]].

test(function_sort_bind_variables_one_var):-
    findall(Picked, 
        function_sort_bind_variables(V-V, _, [x1,Y], [[x1,x2], [y1,y2]], Picked),
        AllPicks2), 
    AllPicks2 = [[x1,y1],[x1, y2]].

test(function_sort_bind_variables_two_var):-
    findall(Picked, 
        function_sort_bind_variables(V-V, _, [X,Y], [[x1,x2], [y1,y2]], Picked),
        AllPicks1), AllPicks1 = [[x1,y1],[x1, y2], [x2,y1], [x2,y2]].



test(ground_compound):-
    findall(GC, ground_compound(V-V, _, (a(X,Y), b(X)), GC),
        GCs), GCs = [(a(x1,y1),b(x1)),(a(x1,y2),b(x1)),
                     (a(x2,y1),b(x2)),(a(x2,y2),b(x2))].

test(collect_ground_literals):-
    collect_ground_literals((0.6 :: a(X,Y)), GLs),
    GLs = [::(0.6,a(x1,y1)),::(0.6,a(x1,y2)),::(0.6,a(x2,y1)),::(0.6,a(x2,y2))].


test(collect_ground_literals_function):-
    C = (fr(X) <--- a(X,Y),\+b(X)),
    collect_ground_literals(C, GLs),
    GLs = [::(1,fr(x1)),::(1,fr(x2))].


test(ground_program):-
    problog_collect(P),
    ground_program(P, GL, GP), !,
    is_permutation(GL, [
        ::(0.3,b(x1)),
        ::(0.3,b(x2)),

        ::(0.6,a(x1,y1)),
        ::(0.6,a(x1,y2)),
        ::(0.6,a(x2,y1)),
        ::(0.6,a(x2,y2)),

        ::(0.8,prob_fr_err_1(x1,y1)),
        ::(0.8,prob_fr_err_1(x1,y2)),
        ::(0.8,prob_fr_err_1(x2,y1)),
        ::(0.8,prob_fr_err_1(x2,y2))]),
    is_permutation(GP, [
        <---(fr(x1),(a(x1,y1),\+b(x1))),
        <---(fr(x1),(a(x1,y2),\+b(x1))),
        <---(fr(x2),(a(x2,y1),\+b(x2))),
        <---(fr(x2),(a(x2,y2),\+b(x2))),

        <---(prob_fr(x1),(prob_fr_err_1(x1,y1),a(x1,y1),\+b(x1))),
        <---(prob_fr(x1),(prob_fr_err_1(x1,y2),a(x1,y2),\+b(x1))),
        <---(prob_fr(x2),(prob_fr_err_1(x2,y1),a(x2,y1),\+b(x2))),
        <---(prob_fr(x2),(prob_fr_err_1(x2,y2),a(x2,y2),\+b(x2)))]).

:- end_tests(ground).