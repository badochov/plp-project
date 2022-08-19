
:- [grammar].

dlist_append(L1 - T1, T1 - T2, L1 - T2).


dlist_member(E, List-End):-
    List \== End,
    List = [E|_].

dlist_member(E, List-End):-
    List \== End,
    List = [_|Tail],
    dlist_member(E, Tail-End).


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


ground_clause(C, GCs):-
    problog_clause(C, P, Head, Body),
    
    findall(GC, (
        ground_compound(V-V, Vs, Body, BodyG),
        ground_compound(Vs, _, Head, HeadG),
        GC = (P :: HeadG <--- BodyG)), GCs).
    

% Program is a list of clauses of the form "Prob :: Head <--- Conditions"

ground([], []).

ground([P1|PT], GP):-
    ground_clause(P1, GP1),
    ground(PT, GP2),
    append(GP1, GP2, GP).