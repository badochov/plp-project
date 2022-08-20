

0.5 :: szn_spr_sum <--- true.  
0.7 :: sprinkler <--- szn_spr_sum.
0.1 :: rain <--- szn_spr_sum.
0.6 :: rain <--- \+szn_spr_sum.

1 :: wet <--- rain.
1 :: wet <--- sprinkler.
1 :: slippery <--- wet.


