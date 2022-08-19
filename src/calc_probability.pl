:- dynamic cached_probability/2.

% calc_probability(+BDD, -Probability)
% Calculates probability for given BDD.
calc_probability(N, P) :-
    clean_cache,
    calc_probability_(N, P),
    clean_cache.

calc_probability_(leaf(P), P) :- !.
calc_probability_(N, P) :- get_from_cache(N, P), !.
calc_probability_(node(LOBDD, Np, ROBDD), P) :-
        calc_probability_(LOBDD, Lp),
        calc_probability_(ROBDD, Rp),
        P is Rp * Np + Lp * (1 - Np),
        save_to_cache(node(LOBDD, Np, ROBDD), P).

get_from_cache(N, P) :- cached_probability(N, P).
save_to_cache(N, P) :- asserta(cached_probability(N, P)).
clean_cache:- retractall(cached_probability(_, _)).

    
