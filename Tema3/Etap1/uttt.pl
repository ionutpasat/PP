:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').
% student file for Ultimate Tic Tac Toe implementation

posToNum(nw, 0). posToNum(n, 1). posToNum(ne, 2).
posToNum(w, 3). posToNum(c, 4). posToNum(e, 5).
posToNum(sw, 6). posToNum(s, 7). posToNum(se, 8).
numToPos(0, nw). numToPos(1, n). numToPos(2, ne).
numToPos(3, w). numToPos(4, c). numToPos(5, e).
numToPos(6, sw). numToPos(7, s). numToPos(8, se).

%indexes of E in List => Is
indexOf(List, E, Is) :-
    findall(N, nth0(N, List, E), Is).

%replace la indexul I in lista L elementul E cu rezultatul in Res
replace(I, L, E, Res) :-
  nth0(I, L, _, R),
  nth0(I, Res, E, R).

%suma unei liste
list_sum([],0).
list_sum([Head|Tail], Sum):-
            list_sum(Tail, Sum1),
            Sum is Head + Sum1.

%predicat folosit pentru a numara X si 0 de pe tabla
checkX(x, 1).
checkX(0, 0).
checkX('', 0).
check0(0, 1).
check0(x, 0).
check0('', 0).

%functii care numara X si 0 de pe tabla
getBoardXCount(B, Count) :- maplist(checkX, B, L), list_sum(L, Count).
getBoard0Count(B, Count) :- maplist(check0, B, L), list_sum(L, Count).

%intoarce nextPlayer in functie de nr de X si 0 de pe tabla
isNextPlayer(N, 0) :- N > 0.
isNextPlayer(N, x) :- N < 0.
isNextPlayer(0, x).

% initialState/1
% initialState(-State)
% Este adevărat pentru starea inițială a jocului.
initialState(S) :- empty_board(Mt), positions(Pos),
            %UBoard, UBoardState, CurrentPlayer, NextMoves, PreviousPosMoved
            S = [[Mt, Mt, Mt, Mt, Mt, Mt, Mt, Mt, Mt], Mt, x, Pos, c].

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
getBoard(S, UPos, B2) :- posToNum(UPos, NPos), getBoards(S, B1), nth0(NPos, B1, B2).

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
getPos(S, UPos, Pos, Cell) :- posToNum(UPos, NUPos), posToNum(Pos, NPos), nth0(0, S, B),
                                nth0(NUPos, B, OneBoard), nth0(NPos, OneBoard, Cell), Cell \= r.

% getPos/3
% getPos(+Board, +Pos, -Cell).
% Este adevărat dacă în tabla individuală reprezentată în Board, la poziția Pos, 
% se află simbolul Cell (x, 0, sau ''). Predicatul poate fi folosit și pentru UBoard, caz 
% în care Cell poate fi și r.
getPos(B, Pos, Cell) :- posToNum(Pos, NPos), nth0(NPos, B, Cell).

% getNextPlayer/2
% getNextPlayer(+State), -NextPlayer)
% Este adevărat dacă în starea State, jucătorul care urmează este NextPlayer
% (poate fi x sau 0)..
getNextPlayer(S, NP) :- nth0(2, S, NP).

% getNextAvailableBoards/2
% getNextAvailableBoards(+State, -NextBoardsPoss)
% Este adevărat dacă în starea State, pozițiile din NextBoardsPoss sunt pozițiile 
% din UBoard ale tablelor disponibile pentru următoarea mutare.
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
buildState(B, P, S) :- posToNum(P, Pos), nth0(Pos, B, BCheck),
                    getBoardResult(BCheck, Res), !, boardCheckBuild(Res, B, P, S).

%calculam nextPlayer in functie de nr de X si 0 de pe tabla
searchNextPlayer(Board, NP) :- maplist(getBoardXCount, Board, XList),
                            maplist(getBoard0Count, Board, List0),
                            list_sum(XList, XCount), list_sum(List0, Count0),
                            Dif is XCount - Count0,
                            isNextPlayer(Dif, NP).

%intoarce o stare in functie de starea tablei curente | folosit la buildState
boardCheckBuild('', B, Pos, S) :- maplist(getBoardResult, B, UBoardState), searchNextPlayer(B, NP), !,
                        S = [B, UBoardState, NP, [Pos], Pos].
boardCheckBuild(_, B, Pos, S) :- maplist(getBoardResult, B, UBoardState), searchNextPlayer(B, NP),
                        indexOf(UBoardState, '', NextMoves), maplist(numToPos, NextMoves, NextMoves1), !,
                        S = [B, UBoardState, NP, NextMoves1, Pos].

% validMove/2
% validMove(+State, +Move)
% Este adevărat dacă mutarea Move este legală în starea State.
% Move este fie o poziție, în cazul în care este o singură tablă disponibilă
% pentru a următoarea mutare din starea State, fie o pereche de poziții, altfel.
validMove([Board, UBoard , _, NextMoves, _], (M1, M2)) :- myMem(M1, NextMoves),
                    getBoardResult(UBoard, Res1), !, checkBoard(Res1),
                    posToNum(M1, NumPos), nth0(NumPos, Board, MB),
                    indexOf(MB, '', Res), posToNum(M2, NM2),
                    getBoardResult(MB, Res2), checkBoard(Res2),
                    myMem(NM2, Res).

validMove([Board, UBoard , _, NextMoves, PrevPos], Move) :- myMem(PrevPos, NextMoves),
                    getBoardResult(UBoard, Res1), !,
                    checkBoard(Res1), 
                    posToNum(PrevPos, NumPos), nth0(NumPos, Board, MB),
                    indexOf(MB, '', Res), posToNum(Move, NumMove),
                    myMem(NumMove, Res).
                    
%intoarce true daca inca se poate muta pe tabla               
checkBoard(X) :-
    (
        (X=x ; X=0 ; X=r) ->
            false ;
            true
    ).

%myMember din curs
myMem(X, [X|_]) :- !.
myMem(X, [_|T]) :- myMem(X, T).



% makeMove/3
% makeMove(+State, +Move, -NewState)
% Este adevărat dacă în urma aplicării mutării Move în starea State
% rezulta starea NewState.
% Move este fie o poziție (din lista positions), în cazul în care nu sunt mai 
% multe table disponibile pentru a următoarea mutare din starea State,
% fie o pereche de poziții, altfel.
%
% Hint: folosiți validMove pentru a verifica mutarea și buildState pentru a construi o stare.
makeMove(S, (M1, M2), New) :- validMove(S, (M1, M2)), posToNum(M1, NM1), posToNum(M2, NM2),
                            nth0(0, S, Boards), nth0(NM1, Boards, IndBoard), nth0(2, S, Player),
                            replace(NM2, IndBoard, Player, RepBoard), replace(NM1, Boards, RepBoard, RepBoards),
                            buildState(RepBoards, M2, New).

makeMove(S, Move, New) :- validMove(S, Move), posToNum(Move, NMove), nth0(4, S, PrevPos), posToNum(PrevPos, NPrevPos),
                        nth0(0, S, Boards), nth0(NPrevPos, Boards, IndBoard), nth0(2, S, Player),
                        replace(NMove, IndBoard, Player, RepBoard), replace(NPrevPos, Boards, RepBoard, RepBoards),
                        buildState(RepBoards, Move, New).

% dummy_first/2
% dummy_first(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din stânga-sus mutare posibilă
% (prima din lista de poziții disponibile).
dummy_first([Boards, _ , _, [NextMove], _], NM) :- posToNum(NextMove, NNM), nth0(NNM, Boards, IndBoard),
                                                    indexOf(IndBoard, '', [NMM | _]), numToPos(NMM, Res),
                                                    NM = Res.
dummy_first([Boards, _ , _, [NM | _], _], (NM1, NM2)) :- posToNum(NM, NNM), nth0(NNM, Boards, IndBoard),
                                                    indexOf(IndBoard, '', [NMM | _]), numToPos(NMM, Res), 
                                                    NM1 = NM, NM2 = Res.

% dummy_last/2
% dummy_last(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din dreapta-jos mutare posibilă 
% (ultima din lista de poziții disponibile).
dummy_last([Boards, _ , _, [NextMove], _], NM) :- posToNum(NextMove, NNM), nth0(NNM, Boards, IndBoard),
                                                    indexOf(IndBoard, '', ResList), reverse(ResList, [NMM | _]), 
                                                    numToPos(NMM, Res), NM = Res.
dummy_last([Boards, _ , _, NextMoves, _], (NM1, NM2)) :- reverse(NextMoves, [NM | _]), posToNum(NM, NNM), nth0(NNM, Boards, IndBoard),
                                                    indexOf(IndBoard, '', ResList), reverse(ResList, [NMM | _]), 
                                                    numToPos(NMM, Res), NM1 = NM, NM2 = Res.
