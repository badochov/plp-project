
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
