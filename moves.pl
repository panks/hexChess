rook(X1,Y1,D,X2,MT,L):-X2 is X1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).

rook(X1,Y1,D,X2,MT,L):-X2 is X1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+2,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-2,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+2,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-2,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).

rook(X1,Y1,D,X2,MT,L):-X2 is X1,Y2 is Y1+3,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-3,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+3,Y2 is Y1+3,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-3,Y2 is Y1+3,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+3,Y2 is Y1-3,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-3,Y2 is Y1-3,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).

rook(X1,Y1,D,X2,MT,L):-X2 is X1,Y2 is Y1+4,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-4,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+4,Y2 is Y1+4,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-4,Y2 is Y1+4,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+4,Y2 is Y1-4,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-4,Y2 is Y1-4,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).

rook(X1,Y1,D,X2,MT,L):-X2 is X1,Y2 is Y1+5,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-5,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+5,Y2 is Y1+5,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-5,Y2 is Y1+5,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+5,Y2 is Y1-5,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-5,Y2 is Y1-5,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).

rook(X1,Y1,D,X2,MT,L):-X2 is X1,Y2 is Y1+6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+6,Y2 is Y1+6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-6,Y2 is Y1+6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+6,Y2 is Y1-6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-6,Y2 is Y1-6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).

rook(X1,Y1,D,X2,MT,L):-X2 is X1,Y2 is Y1+6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+6,Y2 is Y1+6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-6,Y2 is Y1+6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+6,Y2 is Y1-6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-6,Y2 is Y1-6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).

rook(X1,Y1,D,X2,MT,L):-X2 is X1,Y2 is Y1+7,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-7,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+1,Y2 is Y1+7,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-1,Y2 is Y1+7,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+1,Y2 is Y1-7,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-1,Y2 is Y1-7,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).

rook(X1,Y1,D,X2,MT,L):-X2 is X1,Y2 is Y1+8,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-8,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+8,Y2 is Y1+8,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-8,Y2 is Y1+8,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+8,Y2 is Y1-8,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-8,Y2 is Y1-8,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).

rook(X1,Y1,D,X2,MT,L):-X2 is X1,Y2 is Y1+9,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-9,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+9,Y2 is Y1+9,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-9,Y2 is Y1+9,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1+9,Y2 is Y1-9,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
rook(X1,Y1,D,X2,Y2,L):-X2 is X1-9,Y2 is Y1-9,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).

%%%% ---

%bishop
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1+1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1-1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1+1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1-1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).

bishop(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1+4,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-4,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1+2,Y2 is Y1+4,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1-2,Y2 is Y1+4,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1+2,Y2 is Y1-4,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1-2,Y2 is Y1-4,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).

bishop(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1+6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1+3,Y2 is Y1+6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1-3,Y2 is Y1+6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1+3,Y2 is Y1-6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1-3,Y2 is Y1-6,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).

bishop(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1+8,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-8,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1+4,Y2 is Y1+8,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1-4,Y2 is Y1+8,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1+4,Y2 is Y1-8,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1-4,Y2 is Y1-8,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).

bishop(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1+10,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-10,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1+5,Y2 is Y1+10,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1-5,Y2 is Y1+10,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1+5,Y2 is Y1-10,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
bishop(X1,Y1,D,X2,Y2,L):-X2 is X1-5,Y2 is Y1-10,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).