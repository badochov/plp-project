
:- [utils,dlist].

:- op(1120, xfx, <---).
:- op(1200, xfy, ::).
:- dynamic((::)/ 2), dynamic((<---) / 2), dynamic(external/1).
:- discontiguous external/1.
:- discontiguous (<---)/2.
:- discontiguous (::)/2.
:- discontiguous sort/1.

% collect_vars(+Head, +Body, -Vars)
collect_vars(Head, Body, Vars):-
    collect_vars_(Head, V-V, Vars1),
    collect_vars_(Body, Vars1, Vars2),
    dlist_to_list(Vars2, VarsUnunique),
    list_to_ord_set(VarsUnunique, Vars), !.

collect_vars_((\+ A), VarsIn, VarsOut):-
    collect_vars_(A, VarsIn, VarsOut).
collect_vars_((A,B), VarsIn, VarsOut):-
    collect_vars_(A, VarsIn, Vars1),
    collect_vars_(B, Vars1, VarsOut).
collect_vars_((A;B), VarsIn, VarsOut):-
    collect_vars_(A, VarsIn, Vars1),
    collect_vars_(B, Vars1, VarsOut).
collect_vars_(Functor, Vars, VarsOut):-
    Functor =.. [_|HArgs],
    include(var, HArgs, HVars),
    list_to_dlist(HVars, HVarsDL),
    dlist_append(Vars, HVarsDL, VarsOut).

% find_var_sort(+Var, -Sort, +Body)
% Finds the constraints on a variable coming from the body.
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
    linear_eq_ref_search(Var, Args, N),
    same_length(Args, SArgs),
    SA =.. [H|SArgs],
    sort(SA),
    nth0(N, SArgs, Sort), !.


next_id(Atom, AtomID):-
    between(1, 99, ID),
    atom_concat(Atom, '_err_', Atom1),
    atom_concat(Atom1, ID, AtomID),
    \+(_ :: AtomID).

term_expansion( (P :: (Head <--- Body)), [
    (P :: ErrTerm),
    (ErrSort),
    (Head <--- (ErrTerm,Body))
]):-
    collect_vars(Head, Body, Vars),
    maplist({Body}/[V,S]>>find_var_sort(V,S,Body), Vars, ErrSorts),
    Head =.. [H|_],
    next_id(H, ErrID),
    ErrTerm =.. [ErrID|Vars],
    ErrSortHead =.. [ErrID|ErrSorts],
    ErrSort =.. [sort, ErrSortHead].


% iff Clause is a problog clause, give Probability, Head and Body of it.
% problog_clause(+Clause, -Probability, -Head, -Body)
problog_clause(C, 1, Head, Body):-
    C = (Head <--- Body), C, !.

problog_clause(C, P, Head, true):-
    C = (P :: Head), C, !.

% Collect whole problog program
% NOTE: This only works if this code is loaded into the same module as the problog clauses.
% problog_collect(-ProgramAsClauseList)
problog_collect(ProgramAsClauseList):-
    findall(C,
        ((C = (_ <--- _);C = (_ :: _)), C),
        ProgramAsClauseList).