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

big1(
[
  [11,1],
  [1,1],
  [1,21]
]).

big2(
[
  [11],
  [1]
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
getX(1,Y,P,[H|_]):-getY(Y,P1,H),P is P1.
getX(X,Y,P,[H|J]):-X1 is X-1,getX(X1,Y,P1,J),P is P1.

getY(1,P,[H|_]):-P is H.
getY(Y,P,[H|K]):-Y1 is Y-1,getY(Y1,P1,K),P is P1.

testSet(X,Y,P,Dn):-big(D),set(X,Y,P,D,Dn).

%set pos in board
set(X,Y,P,D,Dn):-setX(X,Y,P,D,[],Dn).%last 2 are buffers, next to D is the result!!
setX(1,Y,P,[H|J],T,Re):-set_Y(Y,P,H,Z),append(T,[Z],Re1),append(Re1,J,Re).
setX(X,Y,P,[H|J],T,Re):-X1 is X-1,append(T,[H],Tnew),setX(X1,Y,P,J,Tnew,Re).

set_Y(Y,P,H,Z):-setY(Y,P,H,[],Z).
setY(1,P,[H|B],T,Re):-append(T,[P],Re1),append(Re1,B,Re).%Re1 is [T|P],Re is [Re1|B].
setY(Y,P,[H|B],T,Re):-Y1 is Y-1,append(T,H,Tnew),setY(Y1,P,B,Tnew,Re).

%%%
%rook
rook(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is P//10),rook(X2,Y2,D,[[X2,Y2]|MM],MT).
rook(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is P//10),rook(X2,Y2,D,[[X2,Y2]|MM],MT).
rook(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is P//10),rook(X2,Y2,D,[[X2,Y2]|MM],MT).
rook(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is P//10),rook(X2,Y2,D,[[X2,Y2]|MM],MT).
rook(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is P//10),rook(X2,Y2,D,[[X2,Y2]|MM],MT).
rook(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is P//10),rook(X2,Y2,D,[[X2,Y2]|MM],MT).
rook(X1,Y1,D,MM,MT,L):- MT = MM.

%bishop
bishop(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),bishop(X2,Y2,D,[[X2,Y2]|MM],MT).
bishop(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),bishop(X2,Y2,D,[[X2,Y2]|MM],MT).
bishop(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),bishop(X2,Y2,D,[[X2,Y2]|MM],MT).
bishop(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),bishop(X2,Y2,D,[[X2,Y2]|MM],MT).
bishop(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),bishop(X2,Y2,D,[[X2,Y2]|MM],MT).
bishop(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),bishop(X2,Y2,D,[[X2,Y2]|MM],MT).
bishop(X1,Y1,D,MM,MT,L):- MT = MM.

%knight
knight(X1,Y1,D,MM,MT,L):-X2 is X1+2,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
knight(X1,Y1,D,MM,MT,L):-X2 is X1+2,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
knight(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
knight(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
knight(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1-3,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
knight(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1+3,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
knight(X1,Y1,D,MM,MT,L):-X2 is X1-2,Y2 is Y1-3,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
knight(X1,Y1,D,MM,MT,L):-X2 is X1-2,Y2 is Y1+3,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
knight(X1,Y1,D,MM,MT,L):-X2 is X1-3,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
%knight(X1,Y1,D,MM,MT,L):-MT = MM. MT = [[X2,Y2]|MM]

%queen
queen(X1,Y1,D,MM,MT,L):-rook(X1,Y1,D,MM,MT1,L),bishop(X1,Y1,D,MT1,MT,L).
queen(X1,Y1,D,MM,MT,L):-MT = MM.

%pawn
pawn(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].

%king
king(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1-1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].
king(X1,Y1,D,MM,MT,L):-X2 is X1+1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is P//10),MT = [[X2,Y2]|MM].

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

%play(Turn,Board):-chooseMove(Board,Turn,BoardUp),Turn1 is Turn +1,endGame(BoardUp,Turn1),!,play(Turn1,BoardUp).

% input format : ['piece','row','column] eg: [k,b,5]



play(Turn,Board):-read(X), splitInput(X,X1,Y1,X2,Y2,Turn,Board), move(X1,Y1,X2,Y2,Board,NewBoard), Turn is Turn +1, chooseMove(NewBoard, Turn, NewBoard2), Turn is Turn + 1, play(Turn, NewBoard2). 


ki(k).
qu(q).
ro(r).
kn(k).
bi(b).
pa(p).

one(a).
two(b).
three(c).
four(d).
five(e).
six(f).
seven(g).
eight(h).
nine(i).

isBlack(0).

splitInput([X|[B|[R|D]]], X1, Y1, X2, Y2, Turn, Board):-   (
                ki(X) -> P = 'king', Val = 10;    (
                                            qu(X) -> P='queen', Val = 19;     (
                                      ro(X) -> P='rook', Val = 17 ;    (
                                                        kn(X) -> P='knight', Val = 13;     (
                                                                        bi(X) -> P='bishop', Val = 15 ;     (
                                                                                          pa(X) -> P ='pawn', Val = 11 ; write('invalid piece'), P='', Val = 0
                                                                                    )
                                                                       )
                                                        )
                                                                        )
                                            )
                ), T1 is Turn mod 2,checkforcolor(T1, Val, Val2) , get(Xa, Ya, Val2, Board), 
                
                (
                one(B) -> C = 1 ;    (
                                            two(B) -> C = 2;     (
                                                                    three(B) -> C = 3 ;    (
                                                  four(B) -> C = 4;     (
                                                             five(B) -> C = 5 ;    (
                                                                          six(B) -> C = 6;     (
                                                                                    seven(B) -> C = 7 ;     (
                                                             eight(B) -> C = 8 ;    (
                                                                          nine(B) -> C = 9; write('not a valid column number'), C=0
                                                                          )
                                                            )
                                                                                    )
                                                                          )
                                                            )
                                                )
                                                                 )
                                      )
                ), X1 = Xa, Y1 = Ya, X2 = C, Y2 = R.

checkforcolor(T1, ValIn, ValOut):- ( isBlack(T1) -> ValOut is ValIn + 10 ; ValOut = ValIn ).





%%fill this ; check this for mate!!!
endGame(Board,Turn).

testChooseMove(A):-big2(B),iterateOverBoard(A,10,X1,Y1,X2,Y2,B,1,1,1,B),print(X1),print(X2),print(Y1),print(Y2).

%%choosing!!
chooseMove(Board,Turn,BoardUp):-Ta is Turn mod 2,T is Ta+1, iterateOverBoard(_,1,X1,Y1,X2,Y2,Board,T,1,1,Board),move(X1,Y1,X2,Y2,Board,BoardUp).

iterateOverBoard(Score,Depth,X1,Y1,X2,Y2,[H|T],Var,I,1,Board):-iterateOverY(Score1,Depth,X1a,Y1a,X2a,Y2a,H,Var,I,1,Board),I1 is I+1,
                                                               iterateOverBoard(Score2,Depth,_,_,_,_,T,Var,I1,1,Board),Score1 > Score2,
                                                               Score is Score1,X1 is X1a,X2 is X2a,Y1 is Y1a,Y2 is Y2a.
iterateOverBoard(Score,Depth,X1,Y1,X2,Y2,[H|T],Var,I,1,Board):-iterateOverY(Score1,Depth,_,_,_,_,H,Var,I,1,Board),I1 is I+1,
                                                               iterateOverBoard(Score2,Depth,X1a,Y1a,X2a,Y2a,T,Var,I1,1,Board),Score is Score2,
                                                               X1 is X1a,X2 is X2a,Y1 is Y1a,Y2 is Y2a.
iterateOverBoard(Score,Depth,X1,Y1,X2,Y2,[],Var,I,J,Board):-S is 0,Score is S.
iterateOverBoard(Score,0,X1,Y1,X2,Y2,[],Var,I,J,Board):-S is 0,Score is S.

iterateOverY(S,D,X1,Y1,X2,Y2,[H|T],V,I,J,Board):-findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout),Jn is J+1,
                                                 iterateOverY(Sout1,D,I,J,_,_,T,V,I,Jn,Board),Sout1 < Sout,
                                                 S is Sout,X1  is I,X2 is Xd,Y1 is J,Y2 is Yd.
iterateOverY(S,D,X1,Y1,X2,Y2,[H|T],V,I,J,Board):-findMove(I,J,Board,V,D,NewB,_,_,Sout),Jn is J+1,
                                                 iterateOverY(S1,D,X1a,X2a,X2a,Y2a,T,V,I,Jn,Board),S is S1,
                                                 X1 is X1a,X2 is X2a,Y1 is Y1a,Y2 is Y2a.   
iterateOverY(S,D,X1,Y1,X2,Y2,[],V,I,J,Board):-X1 is 1,X2 is 1,Y1 is 1,Y2 is 1,S1 is 0,S is S1.%hack!!!

%% write for findMove
findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout):-get(I,J,P,Board),Tmp is V*10,P == Tmp,king(I,J,Board,[],MT,V),
                                         getBest1(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout is Eval,Xd is Xb,Yd is Yb.
findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout):-get(I,J,P,Board),Tmp1 is V*10,Tmp is Tmp1+1,P == Tmp,pawn(I,J,Board,[],MT,V),
                                         getBest1(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout is Eval,Xd is Xb,Yd is Yb.
findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout):-get(I,J,P,Board),Tmp1 is V*10,Tmp is Tmp1+3,P == Tmp,knight(I,J,Board,[],MT,V),
                                         getBest1(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout is Eval,Xd is Xb,Yd is Yb.
findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout):-get(I,J,P,Board),Tmp1 is V*10,Tmp is Tmp1+5,P == Tmp,bishop(I,J,Board,[],MT,V),
                                         getBest1(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout is Eval,Xd is Xb,Yd is Yb.
findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout):-get(I,J,P,Board),Tmp1 is V*10,Tmp is Tmp1+7,P == Tmp,rook(I,J,Board,[],MT,V),
                                         getBest1(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout is Eval,Xd is Xb,Yd is Yb.
findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout):-get(I,J,P,Board),Tmp1 is V*10,Tmp is Tmp1+9,P == Tmp,queen(I,J,Board,[],MT,V),
                                         getBest1(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout is Eval,Xd is Xb,Yd is Yb.
findMove(I,J,Board,V,D,NewB,Xd,Yd,Sout):-Sout is 0.

getBest1(I,J,Bb,Board,Xb,Yb,V,D,Score):-getBest(I,J,Bb,Board,Xb1,Yb1,V,D,Score1),Score is Score1,Xb is Xb1,Yb is Yb1.
getBest1(I,J,[H|_],Board,Xb,Yb,V,D,Score):-[X1,Y1] = H,Xb is X1,Yb is Y1,Score is 0.%,getBest(I,J,[H|T],Board,Xb,Yb,V,D,Score1),Score is Score1.
getBest1(I,J,[],Board,Xb,Yb,V,D,Score):-Score is 0.%,getBest(I,J,[H|T],Board,Xb,Yb,V,D,Score1),Score is Score1.

getBest(I,J,[],Board,Xb,Yb,V,Score):-S is 0,Score is S.
getBest(I,J,[H|T],Board,Xb,Yb,V,D,Score):-[X1,Y1] = H,move(I,J,X1,Y1,Board,Bnew),V2 is V+1,V1 is V2 mod 2, V3 is V1+1,D1 is D-1,eval(Sc,V,Bnew),
                                          iterateOverBoard(Sn,D1,_,_,_,_,Bnew,V3,1,1,Bnew),Score is Sn+Sc,
                                          getBest(I,J,T,Board,Xb,Yb,V,D,Snext),
                                          Score > Snext,
                                          Xb is X1,Yb is Y1. 
getBest(I,J,[H|T],Board,Xb,Yb,V,D,Score):-getBest(I,J,T,Board,Xb1,Yb1,V,D,Score1),Xb is Xb1,Yb is Yb1,Score is Score1.

%%write for eval
testEval(Score,T):-big1(B),eval(Score,T,B).

eval(Score,T1,[H|T]):-evalY(S1,T1,H),eval(S2,T1,T),Stmp is S1+S2,Score is Stmp. 
eval(Score,T,[]):-S is 0,Score is S.

evalY(S,T1,[H|T]):-H >=  T1*10,Snew is H mod 10,evalY(Snext,T1,T),Stmp is Snew + Snext,S is Stmp.
evalY(S,T1,[H|T]):-H < 10,Snew is 0,evalY(Snext,T1,T),Stmp is Snew + Snext,S is Stmp.
evalY(S,T1,[H|T]):-evalY(Snext,T1,T),Stmp is Snext + 0,S is Stmp.
evalY(S,T1,[]):-S1 is 0,S = S1.
/* EXPAND THIS TODO */