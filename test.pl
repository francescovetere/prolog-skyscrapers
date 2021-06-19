leggi_lista_str(L, F):- see(F), leggi_lista_str(L), seen.
leggi_lista_str([S|R]):- leggi_atomo(S), S\==end_of_file, !, leggi_lista_str(R).
leggi_lista_str([]).
leggi_atomo(end_of_file):- at_end_of_stream, !.
leggi_atomo(S):- leggi_lista_car(L), name(S, L).
leggi_lista_car([C|R]):- get0(C), C\==10, C\==32, C\==(-1), !, leggi_lista_car(R).
leggi_lista_car([]).


build_matrix(File, Matrix) :- leggi_lista_str([H | T], File), list_to_matrix(T, H, Matrix).

list_to_matrix([], _, []).
list_to_matrix(List, Size, [Row|Matrix]):-
  list_to_matrix_row(List, Size, Row, Tail),
  list_to_matrix(Tail, Size, Matrix).

list_to_matrix_row(Tail, 0, [], Tail).
list_to_matrix_row([Item|List], Size, [Item|Row], Tail):-
  NSize is Size-1,
  list_to_matrix_row(List, NSize, Row, Tail).