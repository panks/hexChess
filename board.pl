
include('moves.pl').
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

Starting Board
big(
[

1 [17,13,15,19,10,15,13,15,17],
2 [11,11,11,11,11,11,11,11,11],
3 [ 1, 1, 1, 1, 1, 1, 1, 1, 1],
4 [ 1, 1, 1, 1, 1, 1, 1, 1, 1],
5 [21, 1, 1, 1, 1, 1, 1, 1,21],
6 [27,21, 1, 1, 1, 1, 1,21,27],
7 [ 0,25,21, 1, 1, 1,21,23, 0],
8 [ 0, 0,23,21, 1,21,25, 0, 0],
9 [ 0, 0, 0,25,25,29, 0, 0, 0],
10[ 0, 0, 0, 0,20, 0, 0, 0, 0]
    a  b  c  d  e  f  g  h  i 
]).

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

big3(
[
  [1,1,1,1,1,1,1,1,1],
  [1,1,1,1,1,1,1,1,13],
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
%setY(1,P,[H|B],[],Re):-append([],[P],Re1),append(Re1,B,Re).%Re1 is [T|P],Re is [Re1|B].
setY(1,P,[H|B],T,Re):-append(T,[P],Re1),append(Re1,B,Re).%Re1 is [T|P],Re is [Re1|B].
setY(Y,P,[H|B],T,Re):-Y1 is Y-1,append(T,[H],Tnew),setY(Y1,P,B,Tnew,Re).


%%%
%knight
knight(X1,Y1,D,X2,Y2,L):-X2 is X1+2,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
knight(X1,Y1,D,X2,Y2,L):-X2 is X1+2,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
knight(X1,Y1,D,X2,Y2,L):-X2 is X1+1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
knight(X1,Y1,D,X2,Y2,L):-X2 is X1+1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
knight(X1,Y1,D,X2,Y2,L):-X2 is X1-1,Y2 is Y1-3,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
knight(X1,Y1,D,X2,Y2,L):-X2 is X1-1,Y2 is Y1+3,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
knight(X1,Y1,D,X2,Y2,L):-X2 is X1-2,Y2 is Y1-3,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
knight(X1,Y1,D,X2,Y2,L):-X2 is X1-2,Y2 is Y1+3,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
knight(X1,Y1,D,X2,Y2,L):-X2 is X1-3,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
%knight(X1,Y1,D,X2,Y2,L):-MT = MM. MT = [[X2,Y2]|MM]

%queen

queen(X1,Y1,D,X2,Y2,L):-rook(X1,Y1,D,X2,Y21,L),(X2 \== X1;Y2 \== Y1).
queen(X1,Y1,D,X2,Y2,L):-bishop(X1,Y1,D,MT1,MT,L),(X2 \== X1;Y2 \== Y1).
%queen(X1,Y1,D,X2,Y2,L):-MT = MM.

%pawn
pawn(X1,Y1,D,X2,Y2,L):-X2 is X1+1,Y2 is Y1,get(X2,Y2,P,D),(P =1;L is 1),(X2 \== X1;Y2 \== Y1).
pawn(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1+1,get(X2,Y2,P,D),(P//10  == 2;L is 1),(X2 \== X1;Y2 \== Y1).
pawn(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-1,get(X2,Y2,P,D),(P//10  == 2;L is 1),(X2 \== X1;Y2 \== Y1).
pawn(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1+1,get(X2,Y2,P,D),(P//10  == 1;L is 2),(X2 \== X1;Y2 \== Y1).
pawn(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-1,get(X2,Y2,P,D),(P//10  == 1;L is 2),(X2 \== X1;Y2 \== Y1).
pawn(X1,Y1,D,X2,Y2,L):-X2 is X1-1,Y2 is Y1,get(X2,Y2,P,D),(P =1;L is 2),(X2 \== X1;Y2 \== Y1).
pawn(X1,Y1,D,X2,Y2,L):-X2 is X1+2,Y1 > 1,Y1 <9,Y2 is Y1,get(X2,Y2,P,D),(P =1;L is 1),(X2 \== X1;Y2 \== Y1).
pawn(X1,Y1,D,X2,Y2,L):-X2 is X1-2,Y1 > 1,Y1 <9,Y2 is Y1,get(X2,Y2,P,D),(P =1;L is 2),(X2 \== X1;Y2 \== Y1).
pawn(X1,Y1,D,X2,Y2,L):-X2 is X1+3,Y1 > 3,Y1 <7,Y2 is Y1,get(X2,Y2,P,D),(P =1;L is 1),(X2 \== X1;Y2 \== Y1).
pawn(X1,Y1,D,X2,Y2,L):-X2 is X1-3,Y1 > 3,Y1 <7,Y2 is Y1,get(X2,Y2,P,D),(P =1;L is 2),(X2 \== X1;Y2 \== Y1).
%add attack moves...

%king
king(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
king(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
king(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
king(X1,Y1,D,X2,Y2,L):-X2 is X1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
king(X1,Y1,D,X2,Y2,L):-X2 is X1+1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
king(X1,Y1,D,X2,Y2,L):-X2 is X1+1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
king(X1,Y1,D,X2,Y2,L):-X2 is X1-1,Y2 is Y1-2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
king(X1,Y1,D,X2,Y2,L):-X2 is X1-1,Y2 is Y1+2,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
king(X1,Y1,D,X2,Y2,L):-X2 is X1-1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
king(X1,Y1,D,X2,Y2,L):-X2 is X1-1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
king(X1,Y1,D,X2,Y2,L):-X2 is X1+1,Y2 is Y1-1,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).
king(X1,Y1,D,X2,Y2,L):-X2 is X1+1,Y2 is Y1+1,get(X2,Y2,P,D),(P =1;L is P//10),(X2 \== X1;Y2 \== Y1).


% user input
try(Q):- Q \== 12.

inputLoop(X):-read(Y),print(Y),loop(Y).
loop(Y):-Y \== 42,read(Y1),print(Y1),loop(Y1).

testStart(Turn):-big1(B),myTurn(1,B).
myTurn(Turn,Board):-read(X),getIpParts(X,X1,Y1,X2,Y2),move(X1,Y1,X2,Y2,Board,NewBoard),Turn1 is Turn + 2,chooseMove(NewBoard, Turn1, NewBoard2), Turn2 is Turn1 + 1, play(Turn2, NewBoard2). 

getIpParts([X,Y,X1,Y1],A,B,C,D):-A is X,B is Y,C is X1,D is Y1.

%play(Turn,Board):-read(X), splitInput(X,X1,Y1,X2,Y2,Turn,Board), move(X1,Y1,X2,Y2,Board,NewBoard), chooseMove(NewBoard, Turn, NewBoard2), Turn is Turn + 2, play(Turn, NewBoard2). 

%TODO Write endgame function
%input usage for moving king from d3 to c5 - [k,d,3,c,5].
%homePlay(T):-big(B),play(1,B),T is 1.
homePlay(T):-big(B),playDual(1,B),T is 2.

play(Turn,Board):- write('Play your move..'),nl, read(X), splitInput2(X,X1,Y1,X2,Y2,Turn,Board), move(X1,Y1,X2,Y2,Board,NewBoard), 
                   Turn2 is Turn + 1, write('Wait for Computer to Play..'), chooseMove(NewBoard, Turn2, NewBoard2), write('Done..'), nl,Turn3 is Turn2 + 1, play(Turn3, NewBoard2). 

%two person!!
playDual(Turn,Board):-player1(Turn,Board,Boardout,Turnout),nl,write('Wait for opponent to Play..'),player1(Turnout,Boardout,Boardout1,Turnout1),nl,write('done'),nl,playDual(1,Boardout1).

/*
playDual(Turn,Board):- write('Play your move..'),nl, read(X), splitInput2(X,X1,Y1,X2,Y2,Turn,Board), move(Y1,X1,Y2,X2,Board,NewBoard), Turn2 is Turn + 1, 
                       write(NewBoard),write('Wait for opponent to Play..'),nl, read(AX), splitInput2(AX,AX1,AY1,AX2,AY2,Turn2,NewBoard),
                       move(AY1,AX1,AY2,AX2,NewBoard,NewBoard2),write('Done..'), nl,Turn3 is Turn2 + 1, write(NewBoard2),playDual(Turn3, NewBoard2). 
*/                       
player1(Turn,Board,NewBoard,Turn2):-write('Play your move..'),nl, read(X), splitInput2(X,X1,Y1,X2,Y2,Turn,Board),isValid(Y1,X1,Y2,X2,Board,Turn), move(Y1,X1,Y2,X2,Board,NewBoard), Turn2 is Turn + 1.

isValid(Y1,X1,Y2,X2,Board,Turn):-pawn(Y1,X1,Board,Y2,X2,Turn).
isValid(Y1,X1,Y2,X2,Board,Turn):-rook(Y1,X1,Board,Y2,X2,Turn).
isValid(Y1,X1,Y2,X2,Board,Turn):-queen(Y1,X1,Board,Y2,X2,Turn).
isValid(Y1,X1,Y2,X2,Board,Turn):-king(Y1,X1,Board,Y2,X2,Turn).
isValid(Y1,X1,Y2,X2,Board,Turn):-bishop(Y1,X1,Board,Y2,X2,Turn).
isValid(Y1,X1,Y2,X2,Board,Turn):-knight(Y1,X1,Board,Y2,X2,Turn).
%TODO Write endgame function
%input usage for moving king from d3 to c5 - [k,d,3,c,5].	

%play(Turn,Board):- write('Play your move..'), big3(Board), read(X), splitInput2(X,X1,Y1,X2,Y2,Turn,Board), move(X1,Y1,X2,Y2,Board,NewBoard), Turn2 is Turn + 1, write('Wait for Computer to Play..'), chooseMove(NewBoard, Turn2, NewBoard2), write('Done..'), Turn3 is Turn2 + 1, play(Turn3, NewBoard2). 

ki(k).
qu(q).
ro(r).
kn(n).
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


                
                

splitInput2([X|[M|[N|[B|[R|D]]]]], X1, Y1, X2, Y2, Turn, Board):-   (
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
                ), T1 is Turn mod 2,checkforcolor(T1, Val, Val2) , Xa is 1, Ya is 2,
                
                getint(B,C), getint(M, T),
                
             X1 = T, Y1 = N, X2 = C, Y2 = R.



getint(B, R):-                (
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
                ), R is C.                
                

checkforcolor(T1, ValIn, ValOut):- ( isBlack(T1) -> ValOut is ValIn + 10 ; ValOut = ValIn ).





%%fill this ; check this for mate!!!
endGame(Board,Turn).

testChooseMove(A):-big1(B),iterateOverBoard(A,2,X1,Y1,X2,Y2,B,1,1,1,B),print(X1),nl,print(Y1),nl,print(X2),nl,print(Y2),nl.

%%choosing!!
%chooseMove(Board,Turn,BoardUp):-Ta is Turn mod 2,T is Ta+1, iterateOverBoard(_,1,X1,Y1,X2,Y2,Board,T,1,1,Board),move(X1,Y1,X2,Y2,Board,BoardUp).
/*
XXX TODO : oneMove(Board,T,D,M,3,2) -> to oneMove(Board,T,D,M,X,Y):: the correct X Y dimension!!!
*/
chooseMove(Board,Turn,BoardUp):-flip(T,Turn),oneMove(Board,T,1,M,3,2),maximumVal(X1,Y1,X2,Y2,-1,M),move(X1,Y1,X2,Y2,Board,BoardUp).

testOneMove(V,D,Im,Jm,M):-big1(B),oneMove(B,V,D,M,Im,Jm).

maximumVal(X1,Y1,X2,Y2,S,[]):-X1 is -1,X2 is -1,Y1 is -1,Y2 is -1,S is -1.
maximumVal(X1,Y1,X2,Y2,S,[H|T]):-exMax(X1a,Y1a,X2a,Y2a,Sa,H),maximumVal(X1b,Y1b,X2b,Y2b,Sb,T),Sa >=Sb,X1 is X1a,Y1 is Y1a,X2 is X2a,Y2 is Y2a,S is Sa.
maximumVal(X1,Y1,X2,Y2,S,[H|T]):-exMax(X1a,Y1a,X2a,Y2a,Sa,H),maximumVal(X1b,Y1b,X2b,Y2b,Sb,T),Sa<Sb,X1 is X1b,Y1 is Y1b,X2 is X2b,Y2 is Y2b, S is Sb.

exMax(X1a,Y1a,X2a,Y2a,Sa,[]):-X1a is -1,X2a is -1,Y1a is -1,Y2a is -1,Sa is -1.
exMax(X1a,Y1a,X2a,Y2a,Sa,[X1,Y1,X2,Y2,S]):-X1a is X1,Y1a is Y1,X2a is X2 , Y2a is Y2,Sa is S.

oneMove(Board,Var,D,M,Imax,Jmax):-setof(Mov,loopX(1,1,Imax,Jmax,Board,Var,D,Mov),M).
%twoMove(Board,Var,D,M,Imax,Jmax):-setof([X,Y,Xd,Yd,S],findMove(X,Y,Board,Var,D,Xd,Yd,S),M).
%oneMove1(Board,Var,D,Ar):-setof([X1,Y1,X2,Y2,Sout],findMove(X1,X2,Board,Var,D,X2,Y2,Sout),Ar).

loopY(I,J,Imax,Jmax,Board,V,D,Ar):-J<Jmax,findMove(I,J,Board,V,D,Xd,Yd,Sout),J1 is J+1,loopY(I,J1,Imax,Jmax,Board,V,D,Ar1),Sout == -1,Ar = Ar1.%Ar = [[I,J,Xd,Yd,Sout]|Ar1].
loopY(I,J,Imax,Jmax,Board,V,D,Ar):-J==Jmax,findMove(I,J,Board,V,D,Xd,Yd,Sout),Sout == -1,Ar = [].
loopY(I,J,Imax,Jmax,Board,V,D,Ar):-J<Jmax,findMove(I,J,Board,V,D,Xd,Yd,Sout),J1 is J+1,loopY(I,J1,Imax,Jmax,Board,V,D,Ar1),Sout > -1,Ar = [I,J,Xd,Yd,Sout|Ar1].
loopY(I,J,Imax,Jmax,Board,V,D,Ar):-J==Jmax,findMove(I,J,Board,V,D,Xd,Yd,Sout),Sout > -1,Ar = [I,J,Xd,Yd,Sout].

loopX(I,_,Imax,Jmax,Board,V,D,Ar):-I<Imax,loopY(I,1,Imax,Jmax,Board,V,D,Ar1),I1 is I+1,loopX(I1,1,Imax,Jmax,Board,V,D,Ar2),append(Ar1,Ar2,Ar).%Ar = [Ar1|Ar2].
loopX(I,_,Imax,Jmax,Board,V,D,Ar):-I==Imax,loopY(I,1,Imax,Jmax,Board,V,D,Ar1),Ar = Ar1.

testFindMove(S,I,J,P,X,Y):-big1(B),findMove(I,J,B,P,2,X,Y,S).
testSetOF(I,J,V,MT):-big1(Board),setof([X,Y],pawn(I,J,Board,X,Y,V),MT).

findMove(I,J,Board,V,D,Xd,Yd,Sout):-get(I,J,P,Board),Tmp is V*10,P == Tmp,flip(V1,V),setof([X,Y],king(I,J,Board,X,Y,V1),MT), 
                                    getBest(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout is Eval,Xd is Xb,Yd is Yb.
findMove(I,J,Board,V,D,Xd,Yd,Sout):-get(I,J,P,Board),Tmp1 is V*10,Tmp is Tmp1+1,P == Tmp,flip(V1,V),setof([X,Y],pawn(I,J,Board,X,Y,V1),MT),print(V), 
                                    getBest(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout is Eval,Xd is Xb,Yd is Yb.                                    
findMove(I,J,Board,V,D,Xd,Yd,Sout):-get(I,J,P,Board),Tmp1 is V*10,Tmp is Tmp1+3,P == Tmp,flip(V1,V),setof([X,Y],knight(I,J,Board,X,Y,V1),MT), 
                                    getBest(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout is Eval,Xd is Xb,Yd is Yb.                                    
findMove(I,J,Board,V,D,Xd,Yd,Sout):-get(I,J,P,Board),Tmp1 is V*10,Tmp is Tmp1+5,P == Tmp,flip(V1,V),setof([X,Y],bishop(I,J,Board,X,Y,V1),MT), 
                                    getBest(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout is Eval,Xd is Xb,Yd is Yb.
findMove(I,J,Board,V,D,Xd,Yd,Sout):-get(I,J,P,Board),Tmp1 is V*10,Tmp is Tmp1+7,P == Tmp,flip(V1,V),setof([X,Y],rook(I,J,Board,X,Y,V1),MT), 
                                    getBest(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout is Eval,Xd is Xb,Yd is Yb.                                    
findMove(I,J,Board,V,D,Xd,Yd,Sout):-get(I,J,P,Board),Tmp1 is V*10,Tmp is Tmp1+9,P == Tmp,flip(V1,V),setof([X,Y],queen(I,J,Board,X,Y,V1),MT), 
                                    getBest(I,J,MT,Board,Xb,Yb,V,D,Eval),Sout is Eval,Xd is Xb,Yd is Yb.                                    
findMove(I,J,Board,V,D,Xd,Yd,Sout):-Xd is I,Yd is J,Sout is -1.%hack XXX                                                                        

lisSize([H|T],Cou):-lisSize(T,Cou1),Cou is Cou1+1.
lisSize([],Cou):-Cou is 0.

flip(V1,V):-1 == V mod 2,V1 is 2.
flip(V1,V):-0 == V mod 2,V1 is 1.

getBest(I,J,[H|T],Board,Xb,Yb,V,D,Score):-[X1,Y1] = H,move(I,J,X1,Y1,Board,Bnew),flip(V3,V),D1 is D-1,eval(Sc,V,Bnew),
                                          %iterateOverBoard(Sn,D1,_,_,_,_,Bnew,V3,1,1,Bnew),Sc1 is Sn+Sc,
                                          Sc1 is Sc,
                                          lisSize([H|T],Len),Len == 1,
                                          Score is Sc1,Xb is X1,Yb is Y1.
getBest(I,J,[H|T],Board,Xb,Yb,V,D,Score):-[X1,Y1] = H,move(I,J,X1,Y1,Board,Bnew),flip(V3,V),D1 is D-1,eval(Sc,V,Bnew),
                                          %iterateOverBoard(Sn,D1,_,_,_,_,Bnew,V3,1,1,Bnew),Sc1 is Sn+Sc,
                                          Sc1 is Sc,
                                          getBest(I,J,T,Board,_,_,V,D,Snext),
                                          Sc1 >= Snext,Score is Sc1,Xb is X1,Yb is Y1. 
getBest(I,J,[H|T],Board,Xb,Yb,V,D,Score):-%getBest(I,J,T,Board,Xb1,Yb1,V,D,Score1),Xb is Xb1,Yb is Yb1,Score is Score1.
                                          [X1,Y1] = H,move(I,J,X1,Y1,Board,Bnew),flip(V3,V),D1 is D-1,eval(Sc,V,Bnew),
                                          %Sc1 is Sn+Sc,
                                          getBest(I,J,T,Board,Xb1,Yb1,V,D,Score1),Xb is Xb1,Yb is Yb1,Score is Score1,
                                          Sc1 is Sc,getBest(I,J,T,Board,_,_,V,D,Snext),Sc1 < Snext. 

%testEval(Score,T):-big1(B),eval(Score,T,B).
%eval(Score,_,_):-random(Val),Val2 is Val *10,Val1 is abs(Val2),Score is round(Val1).

%%write for eval
testEval(Score,T):-big3(B),eval(Score,T,B).

eval(Score,T1,B):-evalX(Score,T1,B,1).

evalX(Score,T1,[H|T],X):-evalY(S1,T1,H,X,1),X1 is X+1,evalX(S2,T1,T,X1),Stmp is S1+S2,Score is Stmp. 
evalX(Score,T,[],X):-S is 0,Score is S.

evalY(S,T1,[H|T],W,V):-H >=  T1*10,H < 10+ T1*10,Snew1 is H mod 10,Snew is Snew1*V*W,V1 is V+1,evalY(Snext,T1,T,W,V1),Stmp is Snew + Snext,S is Stmp.
evalY(S,T1,[H|T],W,V):-H < 10,Snew is H*V*W,V1 is V+1,evalY(Snext,T1,T,W,V1),Stmp is Snew + Snext,S is Stmp.
evalY(S,T1,[H|T],W,V):-V1 is V+1,evalY(Snext,T1,T,W,V1),Stmp is Snext + 0,S is Stmp.
evalY(S,T1,[],W,V):-S1 is 0,S = S1.
/* EXPAND THIS TODO */
