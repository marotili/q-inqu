polyOutside(0, 1, top).
polyOutside(1, 2, right).
polyOutside(2, 3, bottom).
polyOutside(3, 0, left).

polyInside(N, N', Inside) :- 
	polyOutside(N, N', Outside), opposite(Outside, Inside).

opposite(left, right).
opposite(top, bottom).
opposite(A, B) :- opposite(B, A).


from(3). to(7).
test(X) :- from(A), to(B), X = A..B.


test(1..1).

#show test/1.