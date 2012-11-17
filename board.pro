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
  [17,13,15,19,10,15,13,15,17],
  [11,11,11,11,11,11,11,11,11],
  [1,1,1,1,1,1,1,1,1],
  [1,1,1,1,1,1,1,1,1],
  [21,1,1,1,1,1,1,1,21],
  [27,21,1,1,1,1,1,21,27],
  [0,25,21,1,1,1,21,23,0],
  [0,0,23,21,1,21,25,0,0],
  [0,0,0,25,25,29,0,0,0],
  [0,0,0,0,20,0,0,0,0]
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

move(X1,Y1,X2,Y2,D,Dnew):-get(X1,Y1,P,D),set(X1,Y1,1,D,D1),set(X2,Y2,P,D1,Dnew).

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

%%%
%rook
rook(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is p//10),rook(X2,Y2,D,[[X2,Y2]|MM],MT).
rook(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is p//10),rook(X2,Y2,D,[[X2,Y2]|MM],MT).
rook(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is p//10),rook(X2,Y2,D,[[X2,Y2]|MM],MT).
rook(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is p//10),rook(X2,Y2,D,[[X2,Y2]|MM],MT).
rook(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is p//10),rook(X2,Y2,D,[[X2,Y2]|MM],MT).
rook(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is p//10),rook(X2,Y2,D,[[X2,Y2]|MM],MT).
rook(X1,Y1,D,MM,MT,L):- MT = MM.

%bishop
bishop(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is p//10),bishop(X2,Y2,D,[[X2,Y2]|MM],MT).
bishop(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is p//10),bishop(X2,Y2,D,[[X2,Y2]|MM],MT).
bishop(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is p//10),bishop(X2,Y2,D,[[X2,Y2]|MM],MT).
bishop(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is p//10),bishop(X2,Y2,D,[[X2,Y2]|MM],MT).
bishop(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is p//10),bishop(X2,Y2,D,[[X2,Y2]|MM],MT).
bishop(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is p//10),bishop(X2,Y2,D,[[X2,Y2]|MM],MT).
bishop(X1,Y1,D,MM,MT,L):- MT = MM.

%knight
knight(X1,Y1,D,MM,MT,L):-X2 is X1+2,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
knight(X1,Y1,D,MM,MT,L):-X2 is X1+2,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
knight(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
knight(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
knight(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1-3,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
knight(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1+3,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
knight(X1,Y1,D,MM,MT,L):-X2 is X1-2,Y2 is Y1-3,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
knight(X1,Y1,D,MM,MT,L):-X2 is X1-2,Y2 is Y1+3,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
knight(X1,Y1,D,MM,MT,L):-X2 is X1-3,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
%knight(X1,Y1,D,MM,MT,L):-MT = MM. MT = [[X2,Y2]|MM]

%queen
queen(X1,Y1,D,MM,MT,L):-rook(X1,Y1,D,MM,MT1,L),bishop(X1,Y1,D,MT1,MT,L).
queen(X1,Y1,D,MM,MT,L):-MT = MM.

%pawn
pawn(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L = 1),MT = [[X2,Y2]|MM].

%king
king(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is p//10),MT = [[X2,Y2]|MM].

%%%

% user input
inputLoop(X):-read(Y),print(Y),loop(Y).
loop(Y):-Y \== 42,read(Y1),print(Y1),loop(Y1).
/*
%%%
%generate weight of the pieces...
testWeight(Wei):-big(D),getWeight(D,Wei).

getWeight(D,Weight):-iterX(D,Weight).
iterX([],wei).
iterX([H|T],wei):-iterY(H,wei1),iterX(T,wei2),wei = wei1*wei2.

iterY([],Wei).
iterY([H|T],Wei):-evalWeight(H,R1),iterY(T,R2),Wei = R1*R2.
evalWeight(H,W):-W = H.
*/

play(Turn,Board):-chooseMove(Board,Turn,BoardUp),Turn1 is Turn +1,endGame(BoardUp,Turn1),!,play(Turn1,BoardUp).

%%fill this ; check this for mate!!!
endGame(Board,Turn).

testChooseMove(A):-big(B),iterateOverBoard(0,10,X1,Y1,X2,Y2,Board,T,1,1,B).

%%choosing!!
chooseMove(Board,Turn,BoardUp):-Ta is Turn mod 2,T is Ta+1, iterateOverBoard(0,3,X1,Y1,X2,Y2,Board,T,1,1,Board),move(X1,Y1,X2,Y2,Board,BoardUp).

iterateOverBoard(Score,Depth,X1,Y1,X2,Y2,[H|T],Var,I,1,Board):-iterateOverY(Score1,Depth,X1a,Y1a,X2a,Y2a,H,Var,I,J,Board),Score1 > Score,I1 is I+1,
													   iterateOverBoard(Score1,Depth,X1a,Y1a,X2a,Y2a,T,Var,I1,1,Board),
													   X1 = X1a,X2 = X2a,Y1 = Y1a,Y2 = Y2a.
iterateOverBoard(Score,Depth,X1,Y1,X2,Y2,[H|T],Var,I,1,Board):-iterateOverY(Score1,Depth,X1a,Y1a,X2a,Y2a,H,Var,Board),I1 is I+1,
													   iterateOverBoard(Score,Depth,X1,Y1,X2,Y2,T,Var,I1,1,Board).	
iterateOverBoard(Score,Depth,X1,Y1,X2,Y2,[],Var,I,J,Board).
iterateOverBoard(Score,0,X1,Y1,X2,Y2,[],Var,I,J,Board).

iterateOverY(S,D,X1,Y1,X2,Y2,[H|T],V,I,J,Board):-findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout),S < Sout,Jn is J+1,
                                                 iterateOverY(Sout,D,I,J,Xd,Yd,T,V,I,Jn,Board),
                                                 X1  = I,X2 = Xd,Y1 = J,Y2 = Yd.
iterateOverY(S,D,X1,Y1,X2,Y2,[H|T],V,I,J,Board):-findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout),Jn is J+1,
                                                 iterateOverY(S,D,X1,X2,X2,Y2,T,V,I,Jn,Board).													   
iterateOverY(S,D,X1,Y1,X2,Y2,[],V,I,J,Board).

%%write for eval
testEval(Score):-big(B),eval(Score,B).
eval(Score ,[]).
eval(Score,[H|T]):-evalY(S1,H),eval(S2,T),Stmp is S1+S2,Score is Stmp. 

evalY(S,[]).
evalY(S,[H|T]):-H >19,Snew is (H - 20),evalY(Snext,T),Stmp is Snew + Snext,S is Stmp.
evalY(S,[H|T]):-H >9,Snew is (H - 10),evalY(Snext,T),Stmp is Snew + Snext,S is Stmp.
evalY(S,[H|T]):-H < 10,Snew is 0,evalY(Snext,T),Stmp is Snew + Snext,S is Stmp.

/* EXPAND THIS TODO */

%% write for findMove
findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout):-get(I,J,P,D),tmp is V*10,P == tmp,king(I,J,D,[],MT,V),getBest(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout < Eval,Xb = Xd,Yb = Yd.
findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout):-get(I,J,P,D),tmp1 is V*10,tmp1 is tmp+1,P == tmp,pawn(I,J,D,[],MT,V),getBest(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout < Eval,Xb = Xd,Yb = Yd.
findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout):-get(I,J,P,D),tmp1 is V*10,tmp1 is tmp+3,P == tmp,knight(I,J,D,[],MT,V),getBest(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout < Eval,Xb = Xd,Yb = Yd.
findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout):-get(I,J,P,D),tmp1 is V*10,tmp1 is tmp+5,P == tmp,bishop(I,J,D,[],MT,V),getBest(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout < Eval,Xb = Xd,Yb = Yd.
findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout):-get(I,J,P,D),tmp1 is V*10,tmp1 is tmp+7,P == tmp,rook(I,J,D,[],MT,V),getBest(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout < Eval,Xb = Xd,Yb = Yd.
findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout):-get(I,J,P,D),tmp1 is V*10,tmp1 is tmp+9,P == tmp,queen(I,J,D,[],MT,V),getBest(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout < Eval,Xb = Xd,Yb = Yd.
findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout).

getBest(I,J,[H|T],Board,Xb,Yb,V,D,Score):-[C,D] = H,move(I,J,C,D,Board,Bnew),V1 is V+1,D1 is D-1,eval(Sc,Bnew),
                                                  iterateOverBoard(Sn,D1,X1,Y1,X2,Y2,Bnew,V1,1,1,Bnew),Score = Sn+Sc,getBest(I,J,T,Board,Xb,Yb,V,D).


getBest(I,J,[],Board,Xb,Yb,V).