
0.6 :: a(X,Y).
sort(a([x1,x2], [y1,y2])).
0.3 :: b(X).
sort(b([x1,x2])).

fr(X) <--- a(X,Y),\+b(X).
0.8 :: prob_fr(X) <--- a(X,Y),\+b(X).
0.2 :: prob_fr(X) <--- a(X,Y),b(X).

