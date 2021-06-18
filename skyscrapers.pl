:- use_module(library(clpfd)).

%  member(elem, L) ==> true se L contiene elem
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

removeFirst([], []).
removeFirst([_], []).
removeFirst([_ | T], T). 

removeLast([], []).
removeLast([_], []).
removeLast(X, Y) :- reverse(X, [_ | T]), reverse(T, Y).

% unique(L) ==> true se L contiene valori unici
unique([]).
unique([H | T]) :- \+ member(H, T), unique(T).

% uniqueFirstLast(L) ==> true se L, privata del primo e ultimo elemento (quelli di bordo!) contiene valori unici
uniqueFirstLast(L) :- removeFirstLast(L, LNew), unique(LNew).

% uniqueMatrixFirstLast(M) ==> true se la matrice M contiene liste uniche, private del primo e ultimo elemento (quelli di bordo!)
uniqueMatrixFirstLast([]).
uniqueMatrixFirstLast([H | T]) :- 
		uniqueFirstLast(H), uniqueMatrixFirstLast(T).

% isMatrixSquare(M) ==> true se M è almeno 2x2 ed è quadrata
isMatrixSquare([H | T]) :- len(H, COLS), len([H | T], ROWS), 
													 ROWS >= 2, COLS >= 2, ROWS == COLS.

transposeMatrix([[]|_], []).
transposeMatrix(Matrix, [Row|Rows]) :- transpose_1st_col(Matrix, Row, RestMatrix),
                                 transposeMatrix(RestMatrix, Rows).
transpose_1st_col([], [], []).
transpose_1st_col([[H|T]|Rows], [H|Hs], [T|Ts]) :- transpose_1st_col(Rows, Hs, Ts).




% counts the number of visible skyscrapers on a given sequence
countChangesMax(_, 0).
countChangesMax([0|Rest], Constraint) :- countChangesMax(Rest, 0, Constraint, 0).
countChangesMax([First|Rest], Constraint) :- First  #> 0, countChangesMax(Rest, First, Constraint, 1).

% checks whether the number of visible skyscrapers in given sequence satisfies
% a constraint, starting with some max height and a previous count
countChangesMax([], _, Constraint, Constraint).
countChangesMax([First|Rest], Max, Constraint, Seen) :-
    (First #> Max, Num #= Seen + 1, countChangesMax(Rest,First,Constraint,Num));
    (First #=< Max, countChangesMax(Rest,Max,Constraint,Seen)).

% % countChangesMax(L, RIS) ==> RIS è pari al numero di volte in cui il massimo cambia nella lista L (da sinistra a destra)
% countChangesMax([], 0).
% countChangesMax([_], 1).
% countChangesMax([H | T], RIS) :- 

% isListCompliant(L) ==> true se la lista L rispetta il vincolo del gioco skyscrapers previsto per ogni riga/colonna:
% 											 il primo elemento della lista deve essere uguale al numero di massimi incontrati
%											   lungo il resto della lista
isListCompliant([H | T]) :-  countChangesMax(T, MAXS), H == MAXS. 

checkRuleForward([]).
checkRuleForward([H | T]) :-
		isListCompliant(H), checkRuleForward(T).

checkRuleBackward([]).
checkRuleBackward([H | T]) :-
		reverse(H, HReverse), isListCompliant(HReverse), checkRuleBackward(T).

% isMatrixCompliant(L) ==> true se la matrice M rispetta le regole del gioco skyscrapers
isMatrixCompliant(M) :-
		M = 
		[[0, 3, 2, 1, 3, 2, 0],[3, 3, 4, 5, 1, 2, 2],[4, 1, 2, 4, 3, 5, 1],[3, 2, 1, 3, 5, 4, 2],[2, 4, 5, 1, 2, 3, 2],[1, 5, 3, 2, 4, 1, 3],[0, 1, 2, 4, 2, 4, 0]],

		isMatrixSquare(M), 																				% la matrice deve essere quadrata

		removeFirstLast(M, M1), uniqueMatrixFirstLast(M1),        % controllo che all'interno dei bordi vi siano valori unici:
																															% elimino prima e ultima riga, e controllo se le restanti righe
																															% contengono valori unici a meno di prima e ultima colonna
		
		checkRuleForward(M),
		checkRuleBackward(M),

		transposeMatrix(M, MT),
		
		checkRuleForward(MT),
		checkRuleBackward(MT).

% isMatrixCompliant([[0, 3, 2, 1, 3, 2, 0],[3, 3, 4, 5, 1, 2, 2],[4, 1, 2, 4, 3, 5, 1],[3, 2, 1, 3, 5, 4, 2],[2, 4, 5, 1, 2, 3, 2],[1, 5, 3, 2, 4, 1, 3],[0,1, 2, 4, 2, 4, 0]]).

% ==> true