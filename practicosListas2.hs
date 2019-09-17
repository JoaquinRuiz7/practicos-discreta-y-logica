{--Ejercicios practico de listas ejs extra--}
duplicate::[Int]->[Int]
duplicate [] = []
duplicate (x:xs) = x:x:(duplicate xs);

largoLista::[a]->Int
largoLista [] = 0
largoLista (x:xs) = 1 + largoLista xs;

sumLargos::[[a]]->Int
sumLargos [] = 0
sumLargos (x:xs) = (largoLista x) + sumLargos xs;


{--

--}
agregarAlFinal::a->[a]->[a]
agregarAlFinal x ys = ys++[x];

invR::[a]->[a]
invR = undefined

prefijo::Eq a => [a]->[a]->Bool
prefijo [] _ = True;
prefijo (x:xs) (y:ys) 
        | x == y = prefijo xs ys 
        | not ( x == y ) = False;

{--Ejercicio lista de listas--}


posicionPrimeroQueCumple::[a]->(a->Bool) ->Int
posicionPrimeroQueCumple [] p = 0;
posicionPrimeroQueCumple (x:xs) p
    | p x = 1
    | not (p x) = 1 + posicionPrimeroQueCumple xs p;
{--
ultimoQueCumple::[a]->(a->Bool)->Int
ultimoQueCumple [] p = 0
ultimoQueCumple (x:xs) p
    | p x = 1
    |
--}
{--Ejercicios pizarron--}
append::[a]->[a]->[a]
append [] ys = ys
append  (x:xs) ys = x:(append xs ys );

aplanar::[[a]]->[a]
aplanar [] = []
aplanar (x:xs) = append x (aplanar xs);

reduce::(a->b->b)->b->[a]->b
reduce f b [] = b
reduce f b (x:xs) = f x (reduce f b xs);

invertir::[a]->[a]
invertir [] = []
invertir (x:xs) = append (invertir xs) (x:[]);

esVacia::[a]->Bool
esVacia [] = True;
esVacia _ = False;

primerElemento::[a]->a
primerElemento [] = error "Lista vacia"
primerElemento (x:xs) = x;

cola::[a]->[a]
cola [] = []
cola (x:xs) = xs;

ultimoElemento::Eq a => [a]->a
ultimoElemento [] = error "Lista vacia"
ultimoElemento (x:xs) 
    | xs == [] = x
    | otherwise = ultimoElemento xs;

frente:: Eq a => [a]->[a]
frente [] = []
frente (x:xs) 
    | xs == [] = []
    | otherwise = x:frente xs;

esMiembro::[Int]->Int->Bool
esMiembro [] n = False
esMiembro (x:xs) n 
    | x == n = True
    | otherwise = esMiembro xs n ;
    
cuantos:: Eq a => a -> [a] -> Integer
cuantos o [] = 0
cuantos o (x:xs) 
    | x == o = 1+ cuantos o xs
    | not (x == o) = cuantos o xs;

elementoEnPosicionI::Int->[a]->a
elementoEnPosicionI n [] = error "Lista vacia"
elementoEnPosicionI n (x:xs) 
    | n == 1 = x 
    | otherwise = elementoEnPosicionI (n-1) xs;

posicionQueApareceElElemento::Eq a => a ->[a]->Int
posicionQueApareceElElemento o [] = 0
posicionQueApareceElElemento o (x:xs)
    | x == o = 1
    | otherwise = 1 + posicionQueApareceElElemento o xs;

takeWhile2::(a->Bool)->[a]->[a]
takeWhile2 p [] = []
takeWhile2 p (x:xs)
    | not ( p x ) = []
    | p x = x:takeWhile2 p xs;

dropWhile2::(a->Bool)->[a]->[a]
dropWhile2 p [] = []
dropWhile2 p (x:xs) 
    | p x = dropWhile2 p xs 
    | not (p x) = x:dropWhile2 p xs;
    