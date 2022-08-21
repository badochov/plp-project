
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


% Pick possible truth for ground literals.
pick_TF([], []).
pick_TF([G|GroundLiterals], [TF|TF_List]):-
    ((TF = G-true);(TF = G-false)),
    pick_TF(GroundLiterals, TF_List).


:- [utils].
% Print whole formula truth table.
print_formula_truth_table(GroundLiterals, Formula):-
    length(GroundLiterals, N),
    findall(TF_List, pick_TF(GroundLiterals, TF_List), TF_Lists),
    %pick_TF(GroundLiterals, TF_List), TF_Lists = [TF_List],
    write_canonical(TF_Lists),
    writeln(""),

    N1 is N + 1,
    length(Strings, N1),
    maplist([X]>>(X = "~t~a~t~10+"), Strings),
    strSepCat(Strings, "|", Format),
    maplist(term_string, GroundLiterals, GroundLiteralStrings),
    append(GroundLiteralStrings, ["TF"], Vs1),
    format(Format, Vs1),
    writeln(""),

    maplist({Format,Formula}/[TF_List]>>(
        evaluate_formula(TF_List, Formula, TF),(TF = false;(
            TF = true,
        maplist([_-V,Y]>>(term_string(V, Y)), TF_List, TF_ListP),
        append(TF_ListP, [TF], Vs),
        format(Format, Vs),
        writeln("")))
        ), TF_Lists),

    writeln(""),
    writeln("").