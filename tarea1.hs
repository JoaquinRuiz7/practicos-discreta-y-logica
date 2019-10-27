module Tarea1 where
--Booleanos
andor::Bool -> Bool -> Bool -> Bool
andor b1 b2 b3 = (b1 && b2) || b3;
orand :: Bool -> Bool -> Bool -> Bool
orand b1 b2 b3 = (b1 || b2 ) && b3;
fbool :: Bool -> Bool -> Bool -> Bool
fbool b1 b2 b3 = orand b1 b2 (b1 && b3);
con2 :: Bool -> Bool -> Bool -> Bool
con2 b1 b2 b3 = entonces (not (not ( ( b1 && ( not b2 ) ) ) )) (not b3);
entonces::Bool->Bool->Bool
entonces False _ = True
entonces True b2 = b2;
-----------
--Enteros--
divisibleEntre3::Int->Bool
divisibleEntre3 0 = True
divisibleEntre3 n
    | n < 3 = False
    | n >= 3 = divisibleEntre3 (n-3);

multiplicado::Int->Int->Int
multiplicado a 0 = 0
multiplicado 0 b = 0
multiplicado a b = a + multiplicado a (b-1);

filtradoFuncion :: (Int -> Int) -> (Int -> Bool) -> Int -> Int
filtradoFuncion f p 0 = 0
filtradoFuncion f p n 
    | p ( f n ) = n + filtradoFuncion f p (n-1)
    | not (p ( f n )) = filtradoFuncion f p (n-1);

sum1 :: Int->Int
sum1 0 = 0
sum1 n = ( ( 3*n )-2 ) + sum1 (n-1);

dosala :: Int -> Int
dosala 0 = 1
dosala n = 2* dosala (n-1);

sum2 :: Int -> Int
sum2 0 = dosala 0 
sum2 n = dosala n + sum2 (n-1);

--Listas--
descartarPrimeros::(a -> Bool)->[a]->[a]
descartarPrimeros p [] = []
descartarPrimeros p (x:xs)
        | p x = descartarPrimeros p xs
        | otherwise = x:xs;

iguales::Eq a =>a->a-> Bool
iguales e e2 = ( e == e2 );
contiene::Int->[Int]->Bool
contiene e [] = False
contiene e (x:xs) 
    | e == x = True
    | otherwise = contiene e xs;

exor::[Int]->[Int]->[Int]
exor [] l = l
exor l [] = l
exor l1 l2 = eliminarRepetidos ( (noContieneLista l1 l2)++(noContieneLista l2 l1) );

noContieneLista::[Int]->[Int]->[Int]
noContieneLista l [] = []
noContieneLista [] l2 = [] 
noContieneLista (x:xs) l2 
    |contiene x l2 = noContieneLista xs l2
    |otherwise = x:noContieneLista xs l2;

eliminarRepetidos::[Int]->[Int]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs)
    | contiene x xs = eliminarRepetidos xs
    |otherwise = x:eliminarRepetidos xs;

unir::[a]->[a]->[a]
unir [] l2 = l2
unir  (x:xs) l2 = x:(unir xs l2 );

prefijo::Eq a=>[a]->[a]->Bool
prefijo [] l = True
prefijo l [] = True
prefijo (x:xs) (y:ys)
        | x == y = prefijo xs ys
        | otherwise = False;


--------