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
% ?- skyscrapers('3x3.txt').
%		==> true.
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Lettura della matrice da file %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% leggi_lista_str(L, F) ==> L è la lista letta dal file F
leggi_lista_str(L, F):- see(F), leggi_lista_str(L), seen.
leggi_lista_str([S|R]):- leggi_atomo(S), S\==end_of_file, !, leggi_lista_str(R).
leggi_lista_str([]).
leggi_atomo(end_of_file):- at_end_of_stream, !.
leggi_atomo(S):- leggi_lista_car(L), name(S, L).
leggi_lista_car([C|R]):- get0(C), C\==10, C\==32, C\==(-1), !, leggi_lista_car(R).
leggi_lista_car([]).

% list_to_matrix(List, N, Matrix) ==> Matrix è la matrice N x N costruita a partire da List
list_to_matrix([], _, []).
list_to_matrix(List, Size, [Row|Matrix]):-
	  list_to_matrix_row(List, Size, Row, Tail),
  	list_to_matrix(Tail, Size, Matrix).

list_to_matrix_row(Tail, 0, [], Tail).
list_to_matrix_row([Item|List], Size, [Item|Row], Tail):-
  	NSize is Size-1,
  	list_to_matrix_row(List, NSize, Row, Tail).

% build_matrix(File, Matrix) ==> Matrix è la matrice letta dal file File
build_matrix(File, Matrix) :- leggi_lista_str([H | T], File), list_to_matrix(T, H, Matrix).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% Predicato principale %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% skyscrapers(File) ==> true se File contiene una matrice in accordo con le regole del gioco
skyscrapers(File) :- build_matrix(File, Matrix), isMatrixCompliant(Matrix).