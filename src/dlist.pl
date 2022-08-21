dlist_append(L1 - T1, T1 - T2, L1 - T2):- !.
dlist_append(L1 - T1, Item, L1 - T2):- T1 = [Item|T2].

dlist_member(E, List-End):-
    List \== End,
    List = [E|_].

dlist_member(E, List-End):-
    List \== End,
    List = [_|Tail],
    dlist_member(E, Tail-End).

list_to_dlist(L, DL-T):- append(L, T, DL).
dlist_to_list(L-[], L).