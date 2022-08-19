:- op(1120, xfx, <---).
:- op(1200, xfy, ::).
:- dynamic((::)/ 2), dynamic((<---) / 2), dynamic(external/1).
:- discontiguous external/1.
:- discontiguous (<---)/2.
:- discontiguous (::)/2.
:- discontiguous sort/1.


% iff Clause is a problog clause, give Probability, Head and Body of it.
% problog_clause(+Clause, -Probability, -Head, -Body)
problog_clause(C, P, Head, Body):-
    C = (P :: Head <--- Body), C, !.

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