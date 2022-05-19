
:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').
% student file for Ultimate Tic Tac Toe implementation

% initialState/1
% initialState(-State)
% Este adevărat pentru starea inițială a jocului.
initialState(S) :- empty_board(Mt), positions(Pos),
            S = [[Mt, Mt, Mt, Mt, Mt, Mt, Mt, Mt, Mt], Mt, x, Pos].

getNumPos(nw, 0). getNumPos(n, 1). getNumPos(ne, 2).
getNumPos(w, 3). getNumPos(c, 4). getNumPos(e, 5).
getNumPos(sw, 6). getNumPos(s, 7). getNumPos(se, 8).
getPosNum(0, nw). getPosNum(1, n). getPosNum(2, ne).
getPosNum(3, w). getPosNum(4, c). getPosNum(5, e).
getPosNum(6, sw). getPosNum(7, s). getPosNum(8, se).
% getBoards/2
% getBoards(+State, -Boards)
% Este adevărat dacă în starea State, informațiile din tablele individuale sunt
% cele din variabila Boards.
% Boards este legată la o listă de 9 elemente, fiecare element reprezentând o tablă.
% Ordinea tablelor este cea din lista positions (din utils.pl).
% Fiecare element din listă este o listă de 9 elemente, reprezentând
% pozițiile de pe tablă, ca x, 0, sau ''.
% Pozițiile sunt în ordinea din lista positions (din utils.pl).
getBoards(S, B) :- nth0(0, S, B).

% getBoard/3
% getBoard(+State, +UPos, -Board)
% Este adevărat dacă în starea State, la poziția UPos din tabla de UTTT, 
% se află tabla individuală cu reprezentarea din Board.
% Reprezentarea tablei este descrisă în predicatul getBoards/2.
getBoard(S, UPos, B2) :- getNumPos(UPos, RPos), getBoards(S, B1), nth0(RPos, B1, B2).

% getUBoard/2
% getUBoard(stare(+Board, +UboardState, +Player, +NextMoves),
% -UboardState)
% Întoarce reprezentarea UBoard-ului, indicând tablele individuale câștigate,
% remizate, sau încă în desfășurare. Reprezentarea este aceeași ca a tablelor
% individuale (vezi getBoards/2).
getUBoard(S, UB) :- nth0(1, S, UB).

% getPos/4
% getPos(+State, +UPos, +Pos, -Cell).
% Este adevărat dacă în starea State, în tabla individuală de la poziția UPos în UBoard,
% la poziția Pos pe tablă, se află simbolul Cell (x, 0, sau '').
getPos(S, UPos, Pos, Cell) :- getNumPos(UPos, RUPos), getNumPos(Pos, RPos), nth0(0, S, B),
                                nth0(RUPos, B, OneBoard), nth0(RPos, OneBoard, Cell), Cell \= r.

% getPos/3
% getPos(+Board, +Pos, -Cell).
% Este adevărat dacă în tabla individuală reprezentată în Board, la poziția Pos, 
% se află simbolul Cell (x, 0, sau ''). Predicatul poate fi folosit și pentru UBoard, caz 
% în care Cell poate fi și r.
getPos(B, Pos, Cell) :- getNumPos(Pos, RPos), nth0(RPos, B, Cell).

% getNextPlayer/2
% getNextPlayer(+State), -NextPlayer)
% Este adevărat dacă în starea State, jucătorul care urmează este NextPlayer
% (poate fi x sau 0)..
getNextPlayer(S, NP) :- nth0(2, S, NP).

% getNextAvailableBoards/2
% getNextAvailableBoards(+State, -NextBoardsPoss)
% Este adevărat dacă în starea State, pozițiile din NextBoardsPoss sunt pozițiile 
% din UBoard ale tablelor disponibile pentru următoarea mutare.
%%plus1(N, Acc) :- append(Acc, N1, Acc), N1 is N + 1.
%%isAvailable(Board, N, Res) :- N <= 7, nth0(N, Board, AppR), AppR =:= '',
%%                        append(Res, [N], ResT), N1 is N + 1, isAvailable(Board, N1, ResT).
%pred1('', no).
%pred1(_, yes).
%pred2('')
%foldl1(_, [E], E).
%foldl1(Predicate, [X,Y|Z], Result) :-
%  call(Predicate, X, Y, Ans),
%  foldl1(Predicate, [Ans|Z], Result).
indexOf(List, E, Is) :-
    findall(N, nth0(N, List, E), Is).

getNextAvailableBoards(S, UB) :- nth0(3, S, UB).

% getBoardResult/2
% getBoardResult(+Board, -Result)
% Este adevărat dacă pentru o tablă individuală (sau UBoard) cu reprezentarea
% din Board, rezultatul este Result. Result poate fi:
% x sau 0, dacă jucătorul respectiv a câștigat jocul pe tabla dată;
% r, dacă s-a ajuns la remiză (toate pozițiile au fost completate dar
% tabla nu a fost câștigată);
% '', dacă tabla nu a fost câștigată și nu s-au completat toate pozițiile.
% NOTĂ: este deja definit predicatul player_wins/2 în utils.pl.
getBoardResult(B, x) :- player_wins(P, B), P = x.
getBoardResult(B, 0) :- player_wins(P, B), P = 0.
getBoardResult(B, r) :- getBoardXCount(B, CountX),
                    getBoard0Count(B, Count0), 9 is CountX + Count0.
getBoardResult(_, '').

% buildState/3
% buildState(+Boards, +PreviousPos, -State)
% Este adevărat dacă starea State corespunde stării jocului în care tablele
% individuale sunt cele din lista Boards, iar ultima mutare a fost în 
% poziția PreviousPos într-o tablă individuală.
% NOTĂ: nu contează în care tablă individuală s-a realizat ultima mutare.

%replace(I, L, E, K) :-
%  nth0(I, L, _, R),
%  nth0(I, K, E, R).


list_sum([],0).
list_sum([Head|Tail], Sum):-
            list_sum(Tail, Sum1),
            Sum is Head + Sum1.

checkX(x, 1).
checkX(_, 0).
check0(0, 1).
check0(_, 0).
getBoardXCount(B, Count) :- maplist(checkX, B, L), list_sum(L, Count).
getBoard0Count(B, Count) :- maplist(check0, B, L), list_sum(L, Count).
isNextPlayer(N, 0) :- N > 0.
isNextPlayer(N, x) :- N < 0.
isNextPlayer(0, x).
searchNextPlayer(Board, NP) :- maplist(getBoardXCount, Board, XList),
                            maplist(getBoard0Count, Board, List0),
                            list_sum(XList, XCount), list_sum(List0, Count0),
                            Dif is XCount - Count0,
                            isNextPlayer(Dif, NP).

boardCheckBuild('', B, Pos, S) :- maplist(getBoardResult, B, UBoardState), searchNextPlayer(B, NP),
                        S = [B, UBoardState, NP, [Pos]].
boardCheckBuild(_, B, _, S) :- maplist(getBoardResult, B, UBoardState), searchNextPlayer(B, NP),
                        indexOf(UBoardState, '', NextMoves), maplist(getPosNum, NextMoves, NextMoves1),
                        S = [B, UBoardState, NP, NextMoves1].
%boardCheckBuild(0,B,P,S) :- S = 10.

buildState(B, P, S) :- getNumPos(P, Pos), nth0(Pos, B, BCheck),
                    getBoardResult(BCheck, Res), boardCheckBuild(Res, B, P, S).
% validMove/2
% validMove(+State, +Move)
% Este adevărat dacă mutarea Move este legală în starea State.
% Move este fie o poziție, în cazul în care este o singură tablă disponibilă
% pentru a următoarea mutare din starea State, fie o pereche de poziții, altfel.
validMove(S, Moves) :- nth0(3, S, NextMoves), indexOf(NextMoves, '', Res), nth0(0, S, Board),
                       getBoardResult(Board, Res1), length(Res, Len), validAux(Len, Res1, Moves, NextMoves).
validAux(0, _, _, _) :- false.
validAux(1, '', Move, NextMoves) :- member(Move, NextMoves).
validAux(_, '', (M1, M2), NextMoves) :- member(M1, NextMoves), member(M2, NextMoves).
% makeMove/3
% makeMove(+State, +Move, -NewState)
% Este adevărat dacă în urma aplicării mutării Move în starea State
% rezulta starea NewState.
% Move este fie o poziție (din lista positions), în cazul în care nu sunt mai 
% multe table disponibile pentru a următoarea mutare din starea State,
% fie o pereche de poziții, altfel.
%
% Hint: folosiți validMove pentru a verifica mutarea și buildState pentru a construi o stare.
makeMove(_, _, _) :- false.

% dummy_first/2
% dummy_first(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din stânga-sus mutare posibilă
% (prima din lista de poziții disponibile).
dummy_first(_, _) :- false.

% dummy_last/2
% dummy_last(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din dreapta-jos mutare posibilă 
% (ultima din lista de poziții disponibile).
dummy_last(_, _) :- false.
