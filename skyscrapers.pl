%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File: skyscrapers.pl
% Brief: Implementazione del gioco skyscrapers
% Author: Francesco Vetere
% Date: 2021/06/21
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%% Esempio d'uso %%%%%%%%%%%%%%%%%
% swipl
%
% ?- consult('skyscrapers.pl').
%
% ?- isMatrixCompliant([[0,0,0,0,0],[0,1,2,3,1],[0,3,1,2,0],[2,2,3,1,0],[0,0,0,3,0]]). 
%		==> true.
%
% ?- isMatrixCompliant([[0, 3, 2, 1, 3, 2, 0],[3, 3, 4, 5, 1, 2, 2],[4, 1, 2, 4, 3, 5, 1],[3, 2, 1, 3, 5, 4, 2],[2, 4, 5, 1, 2, 3, 2],[1, 5, 3, 2, 4, 1, 3],[0,1, 2, 4, 2, 4, 0]]). 
% 	==> true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Predicati di utilita' su liste %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% member(elem, L) ==> true se L contiene elem
member(X, [X | _]).
member(X, [_ | T]) :- member(X, T).

% append(L, R, RIS) ==> RIS è la lista L concatenata con R
append([], R, R).
append([H | T], R, [H | TEMP]) :- append(T, R, TEMP).

% reverse(A, B) ==> B è la lista A rovesciata
reverse([], []).
reverse([H | T], RIS) :- reverse(T, TEMP), append(TEMP, [H], RIS).

% length(A, LEN) ==> LEN è pari alla lunghezza della lista A
len([], 0).
len([_ | T], RIS) :- len(T, TEMP), RIS is TEMP + 1.

% removeFirstLast(A, RIS) ==> RIS è la lista A privata del primo e dell'ultimo elemento
removeFirstLast(L, RIS) :- removeFirst(L, TEMP), removeLast(TEMP, RIS).

% removeFirst(A, RIS) ==> RIS è la lista A privata del primo elemento
removeFirst([], []).
removeFirst([_], []).
removeFirst([_ | T], T). 

% removeLast(A, RIS) ==> RIS è la lista A privata dell'ultimo elemento
removeLast([], []).
removeLast([_], []).
removeLast(X, Y) :- reverse(X, [_ | T]), reverse(T, Y).

% unique(L) ==> true se L contiene valori unici
unique([]).
unique([H | T]) :- \+ member(H, T), unique(T).

% uniqueFirstLast(L) ==> true se L, privata del primo e ultimo elemento (quelli di bordo!) contiene valori unici
uniqueFirstLast(L) :- removeFirstLast(L, LNew), unique(LNew).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Predicati di utilita' su matrici %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% uniqueMatrixFirstLast(M) ==> true se la matrice M contiene liste uniche, private del primo e ultimo elemento (quelli di bordo!)
uniqueMatrixFirstLast([]).
uniqueMatrixFirstLast([H | T]) :- 
		uniqueFirstLast(H), uniqueMatrixFirstLast(T).

% isMatrixSquare(M) ==> true se M è almeno 2x2 ed è quadrata
isMatrixSquare([H | T]) :- len(H, COLS), len([H | T], ROWS), 
													 ROWS >= 2, COLS >= 2, ROWS == COLS.

% transposeMatrix(M, MT) ==> MT contiene la trasposta di M
transposeMatrix([[] | _], []).
transposeMatrix(Matrix, [FirstColumn | TransposedRestMatrix]) :- 
		splitMatrix(Matrix, FirstColumn, RestMatrix), % FirstColumn diventa la prima riga nella matrice trasposta
    transposeMatrix(RestMatrix, TransposedRestMatrix).

% splitMatrix(Matrix, FirstColumn, RestOfMatrix) ==> Matrix viene divisa in FirstColumn e RestOfMatrix
splitMatrix([], [], []).
splitMatrix(
		[[FirstEl | RestFirstRow] | OtherRows],   							% Matrix
		[FirstEl | RestFirstCol],  														 	% First column
		[RestFirstRow | RestOtherRows]												 	% Rest of the matrix
) :- splitMatrix(OtherRows, RestFirstCol, RestOtherRows).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Predicati specifici di skyscrapers %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% countChangesMax(L, RIS) ==> RIS è pari al numero di volte in cui il massimo cambia nella lista L (da sinistra a destra) 
countChangesMax([0 | T], RIS) :- countChangesMaxAux(T, 0, 1, RIS).
countChangesMax(L, RIS) :- countChangesMaxAux(L, 0, 0, RIS).

% countChangesMaxAux(L, RIS) ==> RIS è pari al numero di volte in cui il massimo cambia nella lista L (da sinistra a destra)
% 															 con massimo corrente pari a CurrentMax e accumulatore Acc
countChangesMaxAux([H | T], CurrentMax, Acc, RIS) :-
    H > CurrentMax,
		AccNew is Acc + 1,
    countChangesMaxAux(T, H, AccNew, RIS).
 
countChangesMaxAux([H | T], CurrentMax, Acc, RIS) :-
    H =< CurrentMax,
    countChangesMaxAux(T, CurrentMax, Acc, RIS).
 
countChangesMaxAux([], _, Acc, Acc).

% isListCompliant(L) ==> true se la lista L rispetta il vincolo del gioco skyscrapers previsto per ogni riga/colonna:
% 											 il primo elemento della lista deve essere uguale al numero di massimi incontrati
% 											 lungo il resto della lista
isListCompliant([0 | _]). 
isListCompliant([H | T]) :-  countChangesMax(T, MAXS), H == MAXS. 

% checkRuleForward(M) ==> true se la matrice M rispetta il vincolo del gioco skyscrapers per ogni riga, da sx a dx
checkRuleForward([]).
checkRuleForward([H | T]) :-
		isListCompliant(H), checkRuleForward(T).

% checkRuleBackward(M) ==> true se la matrice M rispetta il vincolo del gioco skyscrapers per ogni riga, da dx a sx
checkRuleBackward([]).
checkRuleBackward([H | T]) :-
		reverse(H, HReverse), isListCompliant(HReverse), checkRuleBackward(T).

% isMatrixCompliant(L) ==> true se la matrice M rispetta le regole del gioco skyscrapers
isMatrixCompliant(M) :-
		isMatrixSquare(M), 																				% controllo che la matrice sia quadrata

		removeFirstLast(M, M1), uniqueMatrixFirstLast(M1),        % controllo che all'interno dei bordi vi siano valori unici:
																															% elimino prima e ultima riga, e controllo se le restanti righe
																															% contengono valori unici a meno di prima e ultima colonna
		
		checkRuleForward(M),																			% controllo la regola da sx a dx
		checkRuleBackward(M),																			% controllo la regola da dx a sx

		transposeMatrix(M, MT),
		
		checkRuleForward(MT),																			% traspongo e controllo la regola da sx a dx
		checkRuleBackward(MT).																		% traspongo e controllo la regola da dx a sx