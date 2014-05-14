% base points
points(-100..100).


% data
line(start, 0).
line(end, 50).

rect(big, 20).
rect(small, 3).

%
maxRectWidth(X2) :- 0 < X2, X2 < #max{ X : rect(_, X) }, points(X2).

% points near the line
linePoints(X + X2 - X3) :- line(_, X), maxRectWidth(X2), maxRectWidth(X3).

% all points on the line
pointsOnLine(X) :- X0 <= X, X <= X1
	, line(start, X0), line(end, X1), linePoints(X).

% recursion base
possibleFirsts(0, X, Size) :- 
	linePoints(X), X <= X0, line(start, X0)
	, rect(Size, _)
	.

% take exactly one base
1 { rectsOnLine(0, X, Size) : possibleFirsts(0, X, Size) } 1.

% zero or one to finish recursion
0 { rectsOnLine(N+1, X, Size) : 
	linePoints(X), rect(Size, Test)
	, line(end, X1), X <= X1
	, rect(OldSize, OldWidth), X == OldX + OldWidth

	} 1 
	:- rectsOnLine(N, OldX, OldSize), N < 10. 


% every point on the line needs an rect on top of it
pointOccupied(X) :- 
	  rectsOnLine(N, X0, Size), rect(Size, Width)
	, linePoints(X), X0 <= X, X <= X0 + Width.

:- pointsOnLine(X), not pointOccupied(X).

#show rectsOnLine/3.

