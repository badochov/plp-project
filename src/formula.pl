
:- [grounding].
% Formula.pl
%
% Converts a Grounded Program to logic formula.
% A logic formula will have the following grammar:
% not(A)
% and(A,B)
% or(A,B)

% --- List reduce helpers ---

reduce_or_product([B], B).
reduce_or_product([B|Bodies], Formula):-
    reduce_or_product(Bodies, F2),
    Formula = or(B,F2).


reduce_and_product([B], B).
reduce_and_product([B|Bodies], Formula):-
    reduce_and_product(Bodies, F2),
    Formula = and(B,F2).

% --- sorted_head ---
% Builds a list of `Head-[Body1,Body2]` for the given program.
sorted_head_iter([], H, H).
sorted_head_iter([C|GP], Heads1, Heads3):-
    C = (H <--- B),
    (get_assoc(H, Heads1, DL1, Heads2, DL2);(DL1 = DL-DL, put_assoc(H, Heads1, DL2, Heads2))),
    dlist_append(DL1, [B|T]-T, DL2),
    sorted_head_iter(GP, Heads2, Heads3).


sorted_head(GroundedProgram, List):-
    empty_assoc(Heads1),
    sorted_head_iter(GroundedProgram, Heads1, Heads2),
    assoc_to_list(Heads2, ListOfDL),
    maplist([H-DL, H-L]>>(DL = L-LT, LT = []), ListOfDL, List), !.

% --- rewrite_to_Hubert_syntax(+GroundedThing, -HubertThing) ---
% Rewrites a body into Hubert syntax: not/1, and/2, or/2
rewrite_to_Hubert_syntax(\+ GroundedThing, HubertThing):-
    rewrite_to_Hubert_syntax(GroundedThing, I),
    HubertThing = not(I), !.

rewrite_to_Hubert_syntax((GroundedThingA,GroundedThingB), HubertThing):-
    rewrite_to_Hubert_syntax(GroundedThingA, A),
    rewrite_to_Hubert_syntax(GroundedThingB, B),
    HubertThing = and(A,B), !.

rewrite_to_Hubert_syntax((GroundedThingA;GroundedThingB), HubertThing):-
    rewrite_to_Hubert_syntax(GroundedThingA, A),
    rewrite_to_Hubert_syntax(GroundedThingB, B),
    HubertThing = or(A,B), !.

rewrite_to_Hubert_syntax(true, true_):- !.
rewrite_to_Hubert_syntax(false, false_):- !.

rewrite_to_Hubert_syntax(GroundedThing, GroundedThing):- 
    ground(GroundedThing), !.

% --- Clark Completion ---
% Builds bi-implication list for problog program
clark_completion_iter([], []).
clark_completion_iter([(H-Bs)|HeadAndBodies], [Bi|Biimplications]):-
    maplist(rewrite_to_Hubert_syntax, Bs, BHs),
    reduce_or_product(BHs, Bf),
    Bi = [H,Bf],
    clark_completion_iter(HeadAndBodies, Biimplications).


clark_completion(GroundedProgram, Biimplications):-
    sorted_head(GroundedProgram, HeadAndBodies),
    clark_completion_iter(HeadAndBodies, Biimplications).

% Converts a bi-implication to propositional logic
% A and B are grounded propositional compounds.
split_biimplication(A, B, Formula):-
    % Implication truth table:
    % A | B | A => B
    % T   T     T
    % T   F     F
    % F   T     T
    % F   F     T
    %
    % So: A => B equiv to ~A v B

    % A implies B part
    F1 = or(not(A), B),

    % B implies A part
    F2 = or(not(B), A),

    Formula = and(F1, F2).


program_formula_iter([], []).
program_formula_iter([[H,B]|Biimplications], [F|Formulas]):-
    split_biimplication(H, B, F),
    program_formula_iter(Biimplications, Formulas).


% Convert a GroundedProgram to propositional logic
% GroundedProgram: A list of [Head <--- Body]
program_formula(GroundedProgram, Formula):-
    clark_completion(GroundedProgram, Biimplications),
    program_formula_iter(Biimplications, Formulas),
    reduce_and_product(Formulas, Formula), !.

% Convert an expression to propositional logic
formula(Expression, Formula):-
    findall(G,
        ground_compound(V-V, _, Expression, G),
        Groundings),
    maplist(rewrite_to_Hubert_syntax, Groundings, Formulas),
    reduce_or_product(Formulas, Formula).


% Formula evaluation
evaluate_formula(TF_List, not(A), TF):-
    evaluate_formula(TF_List, A, TF1),
    ((TF1 = true, TF = false);(TF1 = false, TF = true)), !.
evaluate_formula(TF_List, A, TF):-
    member(A-Av, TF_List), TF = Av, !.

evaluate_formula(TF_List, and(A,B), TF):-
    evaluate_formula(TF_List, A, TF1),
    evaluate_formula(TF_List, B, TF2),
    ((TF1 = true,TF2 = true, TF = true);TF = false), !.

evaluate_formula(TF_List, or(A,B), TF):-
    evaluate_formula(TF_List, A, TF1),
    evaluate_formula(TF_List, B, TF2),
    (((TF1 = true;TF2 = true), TF = true); TF = false), !.


assign_to(and(F1, F2), V, A, and(Nf1, Nf2)) :-
    assign_to(F1, V, A, Nf1),
    assign_to(F2, V, A, Nf2), !.
assign_to(or(F1, F2), V, A, or(Nf1, Nf2)) :-
    assign_to(F1, V, A, Nf1),
    assign_to(F2, V, A, Nf2), !.
assign_to(not(F), V, A, not(Nf)) :-
    assign_to(F, V, A, Nf), !.
assign_to(V, V, A, A) :- !.
assign_to(F, _, _, F) :- !.


% Simplifies formula removing falses and trues from the formula.
% simplify_formula(+Formula, -FormulaSimplified)

simplify_formula(not(A), Sf):-
    simplify_formula(A, As),
    (   As = false_ -> Sf = true_;
        As = true_  -> Sf = false_;
        Sf = not(As)
    ), !.

simplify_formula(and(A, B), Sf):-
    simplify_formula(A, As), simplify_formula(B, Bs),
    (
        (As = false_ ; Bs = false_) -> Sf = false_;
        (As = true_) -> Sf = Bs;
        (Bs = true_) -> Sf = As;
        Sf = and(As, Bs)
    ), !.

simplify_formula(or(A, B), Sf):-
    simplify_formula(A, As), simplify_formula(B, Bs),
    (
        (As = true_; Bs = true_) -> Sf = true_;
        (As = false_) -> Sf = Bs;
        (Bs = false_) -> Sf = As;
        Sf = or(As, Bs)
    ), !.

simplify_formula(F, F):-
    ground(F); writeln(F), throw("This is unexpected."), !.


negate(true_, false_).
negate(false_, true_).

% Formula Rewrite due to assertions
% formula_given_evidence(+Formula, +EvidenceFormula, -ChangedFormula)
formula_given_evidence(Formula, EvidenceFormula, ChangedFormula):-
    formula_given_evidence_(Formula, true_, EvidenceFormula, ChangedFormula).

% Value is the true_/false_ value of the Expr in 3rd argument
formula_given_evidence_(F, Value, not(A), FO):-
    negate(Value, V2),
    formula_given_evidence_(F, V2, A, FO), !.

formula_given_evidence_(F, Value, and(A,B), FO):-
    formula_given_evidence_(F, Value, A, FO1),
    formula_given_evidence_(FO1, Value, B, FO), !.

formula_given_evidence_(F, Value, or(A,B), FO):-
    Value = true_ -> (
        % Three choices:
        formula_given_evidence_(F, true_, A, FO1_1),
        formula_given_evidence_(FO1_1, false_, B, FO1),

        formula_given_evidence_(F, false_, A, FO2_1),
        formula_given_evidence_(FO2_1, true_, B, FO2),

        formula_given_evidence_(F, true_, A, FO3_1),
        formula_given_evidence_(FO3_1, true_, B, FO3),

        FO = or(FO1, or(FO2, FO3))
    ); (
        formula_given_evidence_(F, false_, A, FO1),
        formula_given_evidence_(FO1, false_, B, FO)
    ), !.

formula_given_evidence_(F, Value, A, FO):-
    (\+ ground(A)) -> format("~w ~w", "Something went wrong, argument not ground:", A);
        assign_to(F, A, Value, FO1), simplify_formula(FO1, FO),!.
