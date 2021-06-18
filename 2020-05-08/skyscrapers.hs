-- File: skyscrapers.hs
-- Brief: Implementazione del gioco skyscrapers
-- Author: Francesco Vetere 
-- Date: 2020/05/09

-- Esempio d'uso
-- > ghc --make skyscrapers.hs
-- > ./skyscrapers
-- > "Insert file name:"
-- > skyscrapers-5x5.txt
-- > "Matrix compliant"

import System.IO
import Data.List  -- reverse, transpose, nub

-- Il main racchiude la parte "impura", cioè non funzionale pura (I/O)
main = do
    print "Insert file name:"
    file_name <- getLine
    file_content <- readFile file_name  -- leggo tutto il file, ottenendo una lunga stringa

    -- divido il contenuto del file per righe separate da '\n' (tramite la funzione lines)
    -- ogni linea e' analizzata tramite una scanl, che mappa la funzione read :: [Int] sulla 
    -- lista formata dalle singole cifre separate da ' ' (tramite la funzione words)
    -- infine, si elimina la prima lista vuota che viene generata, tramite la funzione tail
    -- si ottiene dunque una matrice, che viene passata alla funzione isMatrixCompliant,
    -- e dunque la print ne stampa il risultato finale

    if (isMatrixCompliant . tail . scanl (\acc x -> (map (read) (words x) :: [Int])) [] . lines $ file_content)
    then print "Matrix compliant"
    else 
        print "Matrix not compliant"

-----------------------------------------------------------------------------

-- Data una lista, calcola il numero di volte che il massimo cambia
countChangesMax :: [Int] -> Int
countChangesMax values = length $ foldl (\ acc x -> if null acc || x > head acc then x:acc else acc) [] values

-- Data una lista, restituisce la lista privata del primo e dell'ultimo elemento
removeFirstLast :: [a] -> [a]
removeFirstLast list =
    if (length list >= 2) then tail . init $ list
    else []

-- Data una lista, controlla se, tolti il primo e l'ultimo elemento (che sono quelli di bordo!), contiene valori unici
uniqueValues :: (Eq a) => [a] -> Bool
uniqueValues list =
    let l_ = removeFirstLast list
    in
        if ((length (nub l_)) == length l_) then True
        else False

-- Data una matrice, controlla se essa e' almeno una 2x2, ed e' quadrata
isMatrixSquare :: [[a]] -> Bool
isMatrixSquare m =
    if (
        length m > 2                    -- almeno 2 righe
        &&
        length (m !! 0) > 2             -- almeno 2 colonne
        &&
        length m == length (m !! 0)     -- numero righe = numero colonne
    )
    then True
    else False

-- Data una lista, controlla che rispetti il vincolo del gioco skyscrapers previsto per ogni riga/colonna:
-- il primo elemento della lista deve essere uguale al numero di massimi incontrati lungo il resto della lista
isListCompliant :: [Int] -> Bool
isListCompliant l =
    if (l !! 0 == (countChangesMax $ tail l)) then True 
    else False

-- Data una matrice, verifica se essa rispetta le regole del gioco skyscrapers
isMatrixCompliant :: [[Int]] -> Bool
isMatrixCompliant m =
    if (
        isMatrixSquare m                                                        -- controllo che la matrice sia quadrata
            &&
        (not $ False `elem` (
            map uniqueValues (removeFirstLast m)                                -- controllo che all'interno dei bordi vi siano valori unici
        ))
            &&
        (not $ False `elem` 
        (   map isListCompliant (removeFirstLast m) ++                          -- controllo la regola da sx a dx
                map isListCompliant (reverse (removeFirstLast m)) ++            -- controllo la regola da dx a sx
                map isListCompliant (removeFirstLast (transpose m)) ++          -- traspongo, e controllo la regola da sx a dx
                map isListCompliant (reverse (removeFirstLast (transpose m)))   -- traspongo, e controllo la regola da dx a sx
        ))
    )
    then True
    else False
