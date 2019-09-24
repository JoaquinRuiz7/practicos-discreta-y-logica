module Listas where 
import PracticoEnteros (esDivisor,modulo,aAlaEne)

agregarAlFinal::a->[a]->[a]
agregarAlFinal x (y:ys) = append (y:ys) [x];

agregarAlFinalRecursivo::a->[a]->[a]
agregarAlFinalRecursivo x [] = x:[]
agregarAlFinalRecursivo x (y:ys) 
    | not (esVacia ys) = append [y] (agregarAlFinalRecursivo x ys)
    | esVacia ys = x:ys;

invR::[Int]->[Int]
invR [] = []
invR (x:xs) = append (invR xs) [x];

posicionPrimeroQueCumple::[a]->(a->Bool) ->Int
posicionPrimeroQueCumple [] p = 0;
posicionPrimeroQueCumple (x:xs) p
    | p x = 1
    | not (p x) = 1 + posicionPrimeroQueCumple xs p;


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
    | x == o = 0
    | x/=o = 1 + posicionQueApareceElElemento o xs
    | xs == [] = largoLista (x:xs);

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

filter2::(a->Bool)->[a]->[a]
filter2 p [] = []
filter2 p (x:xs) 
    | p x = x:filter2 p xs
    | not ( p x ) = filter2 p xs;

aplicaFuncion::(a->a)->[a]->[a]
aplicaFuncion f [] = []
aplicaFuncion f (x:xs) = (f x) : aplicaFuncion f xs;

eliminarElemento::Eq a => a->[a]->[a]
eliminarElemento e l = filter2 ( /= e) l;

eliminarElementoR::Eq a => a->[a]->[a]
eliminarElementoR e [] = [];
eliminarElementoR e (x:xs) 
    | e == x = eliminarElementoR e xs 
    | e /= x = x:eliminarElementoR e xs;

eliminarPrimerAparicion::Eq a => a->[a]->[a]
eliminarPrimerAparicion e [] = []
eliminarPrimerAparicion e (x:xs) 
    | e == x = xs
    | e /= x = x:eliminarPrimerAparicion e xs;
    

nElementos::Int->[a]->[a]
nElementos n [] = []
nElementos n (x:xs)
    | n == 0 = []
    | n > 0 = x:nElementos (n-1) xs


elementosDesdeHasta::Int->Int->[a]->[a]
elementosDesdeHasta n1 n2 [] = []
elementosDesdeHasta n1 n2 l 
    | n1 == n2 = (elementoEnPosicionI n2 l):[]
    | n1 /= n2 = (elementoEnPosicionI n1 l) : elementosDesdeHasta (n1+1) n2 l;

fromTo::Int->Int->[Int]
fromTo n n2 
    | n == n2 = n:[]
    | n < n2 = n:fromTo (n+1) n2;

{--El que se pregunta si es divisor se pasa ultimo por parametro--}
divisoresAux::Int->Int->[Int]
divisoresAux  0 n2 = []
divisoresAux n1 0 = []
divisoresAux n1 n2 
    | ( n2 > 0 && (esDivisor n1 n2) ) = n2 : divisoresAux n1 (n2-1)
    | otherwise = divisoresAux n1 (n2-1);  

divisores::Int->[Int]
divisores n = divisoresAux n n;

primo::Int->Bool
primo n = ( ( esMiembro (divisores n) n  ) &&  ( esMiembro (divisores n) 1 ) ) && ( largoLista (divisores n) == 2 ) ;    

listaPrimos::Int->[Int]
listaPrimos 0 = []
listaPrimos n 
    | primo n = n:listaPrimos (n-1)
    | not ( primo n ) = listaPrimos (n-1);

{--Ejercicios extra--}
duplicate::[Int]->[Int]
duplicate [] = []
duplicate (x:xs) = x:x:(duplicate xs);

largoLista::[a]->Int
largoLista [] = 0
largoLista (x:xs) = 1 + largoLista xs;

sumLargos::[[a]]->Int
sumLargos [] = 0
sumLargos (x:xs) = (largoLista x) + sumLargos xs;

prefijo::Eq a => [a]->[a]->Bool
prefijo [] _ = True;
prefijo (x:xs) (y:ys) 
        | x == y = prefijo xs ys 
        | x /= y  = False;

primeroPosicion::[a] -> (a -> Bool) -> Int
primeroPosicion [] p = 0;
primeroPosicion (x:xs) p 
    | p x = 0
    | not ( p x ) = 1 + primeroPosicion xs p ;

ultimoPosicion::Eq a =>[a]->(a->Bool)->Int
ultimoPosicion [] p = 0
ultimoPosicion (x:xs) p 
    | p x = 1+ultimoPosicion xs p 
    | not ( p x) = 0;

subLista::Eq a => [a] -> [a] -> Bool
subLista [] _ = True
subLista (x:xs) (y:ys) 
    | x == y = subLista xs  ys
    | x /= y = False;

intercalarAux::[a]->[a]->Int->[a]
intercalarAux [] l n = [] ++ l
intercalarAux l [] n = l ++ [] 
intercalarAux (x:xs) (y:ys) n
    | (modulo n 2 == 0 ) = x:intercalarAux xs (y:ys) (n+1)
    | (modulo n 2 /=0 ) = y:intercalarAux (x:xs) ys ( n+1 );

intercalar::[a] -> [a] -> [a]
intercalar l1 l2 = intercalarAux l1 l2 0;

cuadradoPerfectoAux::Int->Int->Bool
cuadradoPerfectoAux n n2 
    | n2 > n = False
cuadradoPerfectoAux n n2 = (aAlaEne n2 2 == n) || cuadradoPerfectoAux n (n2+1);

cuadradoPerfecto::Int->Bool
cuadradoPerfecto n = cuadradoPerfectoAux n 1;

elementAt::[a] -> Int -> a
elementAt [] n = error "-1"
elementAt (x:xs) n 
    | n == 0 = x
    | n>0 = elementAt xs (n-1)

