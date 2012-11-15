/*
a([wr,wn,wb,wq,wk,wb,wn,wb,wr]).
b([wp,wp,wp,wp,wp,wp,wp,wp,wp]).
c([.,.,.,.,.,.,.,.,.]).
d([.,.,.,.,.,.,.,.,.]).
e([.,.,.,.,.,.,.,.,.]).
f([br,bp,.,.,.,.,.,bp,br]).
g([x,bb,bp,.,.,.,bp,bn,x]).
h([x,x,bn,bp,.,bp,bb,x,x]).
i([x,x,x,bb,bp,bq,x,x,x]).
j([x,x,x,x,bk,x,x,x,x]).
*/
big(
[
  [wr,wn,wb,wq,wk,wb,wn,wb,wr],
  [wp,wp,wp,wp,wp,wp,wp,wp,wp],
  [.,.,.,.,.,.,.,.,.],
  [.,.,.,.,.,.,.,.,.],
  [bp,.,.,.,.,.,.,.,bp],
  [br,bp,.,.,.,.,.,bp,br],
  [x,bb,bp,.,.,.,bp,bn,x],
  [x,x,bn,bp,.,bp,bb,x,x],
  [x,x,x,bb,bp,bq,x,x,x],
  [x,x,x,x,bk,x,x,x,x]
]).

% A([a,v,c]).
/*
%%% get position here in X<Y matrix notation!!
pos(X,Y):-X == 1,a(Z),posY(Y,Z).
pos(X,Y):-X == 2,b(Z),posY(Y,Z).
pos(X,Y):-X == 3,c(Z),posY(Y,Z).
pos(X,Y):-X == 4,d(Z),posY(Y,Z).
pos(X,Y):-X == 5,e(Z),posY(Y,Z).
pos(X,Y):-X == 6,f(Z),posY(Y,Z).
pos(X,Y):-X == 7,g(Z),posY(Y,Z).
pos(X,Y):-X == 8,h(Z),posY(Y,Z).
pos(X,Y):-X == 9,i(Z),posY(Y,Z).
pos(X,Y):-X == 10,j(Z),posY(Y,Z).
%pos(X,Y):- print('ERR').
*/
pos(X,Y):-big(D),posX(X,Y,D).

posX(1,Y,[H|_]):-posY(Y,H).
posX(X,Y,[H|J]):-Y1 is X-1,posX(Y1,Y,J).

posY(1,[H|_]):-print(H).
posY(Y,[H|K]):-Y1 is Y-1, posY(Y1,K).

%%% DONW WITH POSITIONS

%%% rules for white pawns

move(X1,Y1,X2,Y2,D,Dnew):-get(X1,Y1,P,D),set(X1,Y1,.,D,D1),set(X2,Y2,P,D1,Dnew).

testGet(X,Y,Z):-big(D),get(X,Y,Z,D).

%get pos from board
get(X,Y,P,D):-getX(X,Y,P,D).
getX(1,Y,P,[H|_]):-getY(Y,P,H).
getX(X,Y,P,[H|J]):-X1 is X-1,getX(X1,Y,P,J).

getY(1,P,[H|_]):-P = H.
getY(Y,P,[H|K]):-Y1 is Y-1,getY(Y1,P,K).

testSet(X,Y,P,Dn):-big(D),set(X,Y,P,D,Dn).

%set pos in board
set(X,Y,P,D,Dn):-setX(X,Y,P,D,[],Dn).%last 2 are buffers, next to D is the result!!
setX(1,Y,P,[H|J],T,Re):-set_Y(Y,P,H,Z),append(T,[Z	],Re1),append(Re1,J,Re).
setX(X,Y,P,[H|J],T,Re):-X1 is X-1,setX(X1,Y,P,J,[T|H],Re).

set_Y(Y,P,H,Z):-setY(Y,P,H,[],Z).
setY(1,P,[H|B],T,Re):-append(T,[P],Re1),append(Re1,B,Re).%Re1 is [T|P],Re is [Re1|B].
setY(Y,P,[H|B],T,Re):-Y1 is Y-1,setY(Y1,P,B,[T|H],Re).

%correctness of moves here..
pawnW(X1,Y1,X2,Y2):-X2 == X1,Y2 == Y+1.
pawnW(X1,Y1,X2,Y2):-X1 == 2,X2 == X1,Y1 > 1,Y1 <9,Y2 == Y+2.
pawnW(X1,Y1,X2,Y2):-X1 == 2,X2 == X1,Y1 > 3,Y1 <7,Y2 == Y+3.

%% rook moves
rookW(X1,Y1,X2,Y2):-X1 == X2,Y2 <= 9,Y2 >=1,X2<7.
rookW(X1,Y1,X2,Y2):-X1 == X2,Y2 <= 8,Y2 >=2,X2<8.
rookW(X1,Y1,X2,Y2):-X1 == X2,Y2 <= 7,Y2 >=3,X2<9.
rookW(X1,Y1,X2,Y2):-X1 == X2,Y2 <= 6,Y2 >=4,X2<10.
rookW(X1,Y1,X2,Y2):-X1 == X2,Y2 == 5,X2==10.
rookW(X1,Y1,X2,Y2):-Y1 == Y2,X2 <= 10,X2 >=1.

rookW(X1,Y1,X2,Y2):-Y1 - Y2 == X1 - X2,Y2 >= 1,Y2 =<9,X2>=1,X2<7.
rookW(X1,Y1,X2,Y2):-Y1 - Y2 == X2 - X1,X2>=1,X2<7.
rookW(X1,Y1,X2,Y2):-Y2 - Y1 == X1 - X2,X2>=1,X2<7.

%% knight moves
knightW(X1,Y1,X2,Y2):-X2 == X1+2,Y2 == Y1-1.
knightW(X1,Y1,X2,Y2):-X2 == X1+2,Y2 == Y1+1.
knightW(X1,Y1,X2,Y2):-X2 == X1+1,Y2 == Y1-2.
knightW(X1,Y1,X2,Y2):-X2 == X1+1,Y2 == Y1+2.
knightW(X1,Y1,X2,Y2):-X2 == X1-1,Y2 == Y1-3.
knightW(X1,Y1,X2,Y2):-X2 == X1-1,Y2 == Y1+3.
knightW(X1,Y1,X2,Y2):-X2 == X1-2,Y2 == Y1-3.
knightW(X1,Y1,X2,Y2):-X2 == X1-2,Y2 == Y1+3.
knightW(X1,Y1,X2,Y2):-X2 == X1-3,Y2 == Y1-2.
knightW(X1,Y1,X2,Y2):-X2 == X1-3,Y2 == Y1+2.
knightW(X1,Y1,X2,Y2):-X2 == X1-3,Y2 == Y1+1.
knightW(X1,Y1,X2,Y2):-X2 == X1-3,Y2 == Y1-1.

%%bishop moves
bishopW(X1,Y1,X2,Y2):-


%and rest so on ...
%%%

% user input
inputLoop(X):-read(Y),print(Y),loop(Y).
loop(Y):-Y \== 42,read(Y1),print(Y1),loop(Y1).




