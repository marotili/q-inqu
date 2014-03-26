#const width = 500.
#const height = 500.


polyline(0,0,0).
polyline(1,60,0).
polyline(2,60,40).
polyline(3,0,40).

inside(0, 1, bottom).
inside(1, 2, left).
inside(2, 3, top).
inside(3, 0, right).

coordinates(-width..width).

tileIds(0..1).
	
tileSize(wall1, 11, 8). % Wall1 / vertical
tileSize(wall1_2, 11, 8). % Wall1_2
tileSize(wall2, 13, 6). % Wall2 / vertical
tileSize(wall2_2, 13, 6).
tileSize(wall3, 10, 9). % Wall3 / horizontal
tileSize(wall3_2, 10, 9).
tileSize(wall4, 12, 9). % Wall4 / horizontal / vertical
tileSize(wall4_2, 12, 9).
tileSize(wall5, 16, 8). % Wall5 / horizontal
tileSize(wall5_2, 16, 8).
tileSize(wall6, 13, 7). % Wall6 / horizontal
tileSize(wall6_2, 13, 7).

tileBaselinePoly(wall1, topLeft, 3, 2).
tileBaselinePoly(wall1, bottomRight, 8, 8).
tileBaselinePoly(wall1_2, topLeft, 3, 2).
tileBaselinePoly(wall1_2, bottomRight, 8, 8).

tileBaselinePoly(wall2, topLeft, 3, 2).
tileBaselinePoly(wall2, bottomRight, 8, 8).
tileBaselinePoly(wall2_2, topLeft, 3, 2).
tileBaselinePoly(wall2_2, bottomRight, 8, 8).

tileBaselinePoly(wall3, topLeft, 3, 2).
tileBaselinePoly(wall3, bottomRight, 8, 8).
tileBaselinePoly(wall3_2, topLeft, 2, 2).
tileBaselinePoly(wall3_2, bottomRight, 7, 8).

tileBaselinePoly(wall4, topLeft, 3, 2).
tileBaselinePoly(wall4, bottomRight, 8, 8).
tileBaselinePoly(wall4_2, topLeft, 2, 2).
tileBaselinePoly(wall4_2, bottomRight, 7, 8).

tileBaselinePoly(wall5, topLeft, 3, 2).
tileBaselinePoly(wall5, bottomRight, 8, 8).
tileBaselinePoly(wall5_2, topLeft, 2, 2).
tileBaselinePoly(wall5_2, bottomRight, 7, 8).

tileBaselinePoly(wall6, topLeft, 3, 2).
tileBaselinePoly(wall6, bottomRight, 8, 8).
tileBaselinePoly(wall6_2, topLeft, 2, 2).
tileBaselinePoly(wall6_2, bottomRight, 7, 8).


polyIds(PId) :- polyline(PId, _, _).

nextPoly(N, X, Y) :- polyline(N+1, X, Y) ; N != 3.
nextPoly(3, X, Y) :- polyline(0, X, Y).

initInstance(0, 0, X, Y, TileId) :-
	coordinates(X), coordinates(Y)
	, polyline(0, X1, Y1)
	, nextPoly(0, X2, Y2)
	, tileBaselinePoly(TileId, bottomRight, BaselineX, BaselineY)
	, X + BaselineX == X1, Y + BaselineY == Y1
	.

initInstance(N, 0, X, Y, TileId) :-
	coordinates(X), coordinates(Y)
	, initInstance(N-1, 0, OldX, OldY, OldTileId)

	, tileBaselinePoly(OldTileId, bottomRight, OldBaselineX, OldBaselineY)
	, tileSize(OldTileId, OldW, OldH)

	, polyline(0, X1, Y1)
	, nextPoly(0, X2, Y1)
	, tileSize(TileId, W, H)
	, tileBaselinePoly(TileId, bottomRight, BaselineX, BaselineY)
	, 1 { X + BaselineX == X2; X + BaselineX < X2 }
	, Y + BaselineY == Y1
	, X >= OldX + OldBaselineX
	, X <= OldX + OldW
	.

1 { instances(N, 0, X, Y, TileId) : initInstance(N, 0, X, Y, TileId) }.

:- instances(N, 0, X, Y, TileId), instances(N, 0, X2, Y2, TileId2)
	, 1 { X != X2; Y != Y2; TileId != TileId2 }
	.

corner(N, 0, X, Y, TileId) :- instances(N, 0, X, Y, TileId)
	, polyline(0, X1, Y1)
	, nextPoly(0, X2, Y1)
	, tileSize(TileId, W, H)
	, tileBaselinePoly(TileId, bottomRight, BaselineX, BaselineY)
	, X + BaselineX == X2
	.

:- instances(N, 0, X, Y, TileId), not corner(N, 0, X, Y, TileId).

#show instances/5.
#show corner/5.
%#show pointsOccupied/2.