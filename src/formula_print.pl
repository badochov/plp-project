

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