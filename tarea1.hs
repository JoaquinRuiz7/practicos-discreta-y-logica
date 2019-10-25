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
divisibleEntre3::Int->Bool
divisibleEntre3 0 = True
divisibleEntre3 n
    | n < 3 = False
    | n >= 3 = divisibleEntre3 (n-3);
--Pre solo b puede ser 0
multiplicado::Int->Int->Int
multiplicado a 0 = 0
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

-- Demostraciones 
--sum1 n = n(3n−1)/2


{--
Paso base ( n = 1)
sum1 1 = 1(3*1-1)/2
= def sum1   = aplicamos * y resta
    3*1-2  =   1(3-1)/2
    3-2    =   1(2)/2
    1      =      1

Paso inductivo ( n = n+1)
Hi) sum1 n = n(3n-1)/2
Ti) sum1 (n+1) = (n+1)(3(n+1)-1)/2
Demostracion:
    sum1 (n+1) = (n+1) (3(n+1)-1)/2
    = def sum1          = distributiva
    3n-2 + sum1 (n-1)  = (n+1) (3n+3-1)/2
    = hi)              = 
    3n-2+n(3n-1)/2     = (n+1) (3n+2)/2
    = distributiva     = distributiva
    3n-2+3n2-n/2       = 3n2+2n+3n+2/2
    3n2-2n-2/2         = 3n2+5n+2/2
Demostracion 2:
 dosala (n + 1) = (sum2 n) + 1
 Paso base (n=0):
 dosala 1 = sum2 1 + 1
 = def dosala       = def sum2
        2*dosala 0 = dosala 0 + 1 
= def dosala        =def dosala 
        2 * 1       = 1+1
        2           =  2
Paso inductivo:
Hi) dosala ( n+1 ) = (sum2 n) + 1
Ti) dosala ( (n+1)+1) = (sum2 (n+1) + 1)

demostracion:
dosala ( (n+1)+1) = (sum2 (n+1) +1)
=sumamos
dosala (n+2)      =  sum2 (n+2) 
=def dosala        def sum2
2*dosala(n+1)     = dosala (n+2) +sum2(n+1)
=hi)              def dosala 
2*sum2 n +1       = 2*dosala(n+1) +sum2(n+1) 
                  hi)
                  =2*sum2 n +1 + sum2(n+1)
--}
--Listas
descartarPrimeros::(a -> Bool)->[a]->[a]
descartarPrimeros p [] = []
descartarPrimeros p (x:xs)
        | p x = descartarPrimeros p xs
        | otherwise = x:xs;

iguales::Eq a =>a->a-> Bool
iguales e e2 = ( e == e2 );
contiene::Eq a =>a->[a]->Bool
contiene e [] = False
contiene e (x:xs)
        | e == x = True
        | otherwise = contiene e xs;

esVacia::[a]->Bool
esVacia [] = True
esVacia _ = False;

exor :: Ord a => [a] -> [a] -> [a]
exor [] l2 = l2
exor l1 [] = l1
exor (x:xs) (y:ys)  
        | (contiene x (y:ys) ) || (contiene y (x:xs))   = exor xs ys
        | esVacia (xs) 
        | otherwise = x:y:exor xs ys;
unir::[a]->[a]->[a]
unir [] l = l
unir l [] = l
unir l l2 = l ++l2
prefijo::Eq a=>[a]->[a]->Bool
prefijo [] l = True
prefijo (x:xs) (y:ys)
        | x == y = prefijo xs ys
        | otherwise = False;


--------