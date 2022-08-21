
:- begin_tests(probability__causal_sprinkler).

:- [probability, utils, problog_examples/causal_sprinkler].

test(query_true):-
    prob(true, P),
    float_cmp(P, 1).

test(query_false):-
    prob(false, P),
    float_cmp(P, 0).

test(query_szn_spr_sum):-
    prob(szn_spr_sum, P),
    float_cmp(P, 0.5).

test(query_sprinkler):-
    prob(sprinkler, P),
    float_cmp(P, 0.35).

test(query_rain):-
    prob(rain, P),
    float_cmp(P, 0.35).

test(query_wet):-
    prob(wet, P),
    float_cmp(P, 0.5985).

test(query_slippery):-
    prob(slippery, P),
    float_cmp(P, 0.4788).

test(query_rain_and_szn_spr_sum):-
    prob((rain, szn_spr_sum), P),
    float_cmp(P, 0.05).

test(query_slippery_wet):-
    prob((wet, slippery), P),
    float_cmp(P, 0.4788).

test(query_slippery_season):-
    prob((slippery, szn_spr_sum), P),
    float_cmp(P, 0.2628).

test(query_all_but_szn):-
    prob((wet, slippery, sprinkler, rain), P),
    float_cmp(P, 0.0252).

test(query_all):-
    prob((wet, slippery, sprinkler, rain, szn_spr_sum), P),
    float_cmp(P, 0.0252).

test(query_rain_if_wet):-
    prob(wet, rain, P),
    float_cmp(P, 0.9).

test(query_wet_if_szn):-
    prob(wet, szn_spr_sum, P),
    float_cmp(P, 0.657).

test(query_wet_if_not_szn):-
    prob(wet, \+szn_spr_sum, P),
    float_cmp(P, 0.343).


:- end_tests(probability__causal_sprinkler).
