
:- [grammar, dlist].


function_sort_bind_variables(Vs, Vs, [], [], []).
function_sort_bind_variables(
    Vars, VarsOut, 
    [A|Args], [SA|SortArgs], [PA|PickedArgs]):-
    (
        var(A) -> (
            (dlist_member((Av : PA), Vars), Av == A, Vars2 = Vars, !);
            (member(PA, SA), dlist_append(Vars, [(A : PA)|VT]-VT, Vars2))
        );
        (atom(A), PA = A, Vars2 = Vars)
    ),
    function_sort_bind_variables(Vars2, VarsOut, Args, SortArgs, PickedArgs).


% Sort groundings
function_sort_grounding(_, C, C):-
    functor(C, _, 0), !.

function_sort_grounding(Vars, VarsOut, C, GC):-
    functor(C, _, N),
    C =.. [H|Args],
    length(SArgs, N), length(Args, N), length(NArgs, N),
    SC =.. [H|SArgs],
    ((sort(SC));(maplist([X]>>(X = []), SArgs))), !,    
    function_sort_bind_variables(Vars, VarsOut, Args, SArgs, NArgs),
    GC =.. [H|NArgs].


% Not
ground_compound(BoundVar, BoundVarOut, C, GC):-
    C = (\+ NotC), !,
    ground_compound(BoundVar, BoundVarOut, NotC, GC1),
    GC = (\+ GC1).

% And 
ground_compound(BoundVar, BoundVarOut, C, GC):-
    (C1,C2) = C, !,
    ground_compound(BoundVar, BoundVarOutI, C1, GC1),
    ground_compound(BoundVarOutI, BoundVarOut, C2, GC2),
    GC = (GC1,GC2).


% Or
ground_compound(BoundVar, BoundVarOut, C, GC):-
    (C1;C2) = C, !,
    ground_compound(BoundVar, BoundVarOutI, C1, GC1),
    ground_compound(BoundVarOutI, BoundVarOut, C2, GC2),
    GC = (GC1;GC2).

% predicate
ground_compound(BoundVar, BoundVarOut, C, GC):-
    !, function_sort_grounding(BoundVar, BoundVarOut, C, GC).


% ground_clause(+Clause, -GroundClauses)
ground_clause(C, GCs):-
    problog_clause(C, P, Head, Body),

    ((Body = true, GCs = []);
    
    findall(GC, (
        ground_compound(V-V, Vs, Body, BodyG),
        ground_compound(Vs, _, Head, HeadG),
        GC = (P :: HeadG <--- BodyG)), GCs)), !.
    
find_var_sort(Var, Sort, (\+ A)):-
    !, find_var_sort(Var, Sort, A).
find_var_sort(Var, Sort, (A,B)):-
    (find_var_sort(Var, Sort, A);
    find_var_sort(Var, Sort, B)), !.
find_var_sort(Var, Sort, (A;B)):-
    (find_var_sort(Var, Sort, A);
    find_var_sort(Var, Sort, B)), !.
find_var_sort(Var, Sort, A):-
    A =.. [H|Args],
    nth0(N, Args, Var),
    length(Args, L),length(SArgs, L),
    SA =.. [H|SArgs],
    sort(SA),
    nth0(N, SArgs, Sort), !.

% True if VarsOut ~ [x1, y2, ...] is a possible binding out of Sorts ~ [[x1,x2], [y1,y2], ...]
pick_binding([], []).
pick_binding([S|Sorts], [VO|VarsOut]):-
    member(VO, S), pick_binding(Sorts, VarsOut).

% if Clause is a function of a sort, return all permutations of groundings.
collect_ground_literals(C, Groundings):-
    problog_clause(C, P, Head, B),
    ((B = true, findall((P :: HeadG),
        ground_compound(V-V, _, Head, HeadG),
        Groundings)
    );(
        Head =.. [H|HArgs],
        maplist({B}/[V,S]>>find_var_sort(V,S,B), HArgs, Sorts), !,
        findall((P :: HeadG),
            (
                pick_binding(Sorts, Bindings),
                HeadG =.. [H|Bindings]
            ),
            Groundings)
    )), !.

% ground_program(+Program, -GroundLiterals, -GroundedProgram)
% Program is a list of clauses of the form "Prob :: Head <--- Conditions"
% GroundLiterals is a list of "(Prob :: Head)" for each ground literal.
ground_program(Program, GroundLiterals, GroundedProgram):-
    ground_iter(Program, UnuniqueGLs, GroundedProgram),
    list_to_ord_set(UnuniqueGLs, GroundLiterals).

ground_iter([], [], []).
ground_iter([P1|PT], GroundLiterals, GP):-
    ground_clause(P1, GP1), collect_ground_literals(P1, GL1),
    ground_iter(PT, GL2, GP2),
    append(GP1, GP2, GP), append(GL1, GL2, GroundLiterals).