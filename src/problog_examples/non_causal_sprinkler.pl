%  non_causal_sprinkler.pl
u1 : 0.35. u2 : 0.23. u3 : 0.1. u4 : 0.6. u5 : 0.9. u6 : 0.8.
sprinkler :- u1.
szn_spr_sum :- sprinkler.
szn_spr_sum :- \+sprinkler, u2.
rain :- szn_spr_sum, u3.
rain :- \+szn_spr_sum, u4.
wet :- rain, u5.
wet :- sprinkler, u5.
slippery :- wet, u6.