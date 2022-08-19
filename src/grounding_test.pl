
:- begin_tests(ground).

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

test(ground_program):-
    findall(C, (C = (_ <--- _), C), P),
    ground(P, GP), 
    GP = [
    ::(1,<---(fr(x1),(a(x1,y1),\+b(x1)))),
    ::(1,<---(fr(x1),(a(x1,y2),\+b(x1)))),
    ::(1,<---(fr(x2),(a(x2,y1),\+b(x2)))),
    ::(1,<---(fr(x2),(a(x2,y2),\+b(x2))))].

:- end_tests(ground).