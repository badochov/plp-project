
% Join string list with separator
% strSepCat(Strings, Separator, StrCat) 
strSepCat([ ],_,Empty) :-
    string_to_list(Empty,[ ]).
strSepCat([H|T],Separator,StrCat) :-
    strSepCat(T,Separator,H,StrCat).

strSepCat([ ],_,StrCat,StrCat).
strSepCat([H|T],Sep,Str,Cat) :-
    string_concat(Sep,H,SepH),
    string_concat(Str,SepH,StrSepH),
    strSepCat(T,Sep,StrSepH,Cat).


% linear_eq_ref_search(+Item, +HayStack, -Nth0)
% Searches linearly through list with == operator.
linear_eq_ref_search(Item, HayStack, Nth0):-
    linear_eq_ref_search_(Item, HayStack, 0, Nth0).

linear_eq_ref_search_(Item, [H|HayStack], NCur, Nth0):-
    H == Item
        -> Nth0 = NCur;
        NCur1 is NCur + 1,
        linear_eq_ref_search_(Item, HayStack, NCur1, Nth0).