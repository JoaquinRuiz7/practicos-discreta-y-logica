module Enteros where 

sumatoria::Int->Int
sumatoria n 
    | n == 0 = 0
    | n>0 = n + sumatoria (n-1);

factorial::Int->Int
factorial n
    | n == 1 = 1
    | n > 0 = n * factorial(n-1);

dosALaEne::Int->Int
dosALaEne n 
    | n == 0 = 1
    | n>0 = 2*dosALaEne(n-1);

aAlaEne::Int->Int->Int
aAlaEne a n
    | n == 0 = 1 
    | n > 0 = a * aAlaEne a (n-1);

copias::Int->String->String
copias n s 
        | n == 1 = s
        | n>0 = s++copias(n-1) s;

multiplicacionConMas::Int->Int->Int
multiplicacionConMas n1 n2 
    | n2 ==0 = 0
    | n2 > 0 = n1 + multiplicacionConMas n1 (n2-1);

divisionConResta::Int->Int->Int
divisionConResta n1 n2 
    | n2 == 0 = error "No existe"
    | n2 > n1 = 0
    | otherwise = 1 + divisionConResta (n1-n2) n2;

modulo::Int->Int->Int
modulo n n2 
        | n == 0 = 0
        | n2 > n = n
        | otherwise = modulo (n-n2) n2;

sumatoriaALaDos::Int->Int 
sumatoriaALaDos n
    | n == 0 = 0
    | otherwise = (aAlaEne n 2)+sumatoriaALaDos (n-1);

sumaPrimerosImpares::Int->Int
sumaPrimerosImpares n
    | n == 0 = 0
    |  n>0 = (n*2-1)+sumaPrimerosImpares (n-1);
    
sumatoriaALaTres::Int->Int 
sumatoriaALaTres n
        | n == 0 = 0
        | otherwise = (aAlaEne n 3)+sumatoriaALaTres (n-1);


sumaEntreAyB::Int->Int->Int
sumaEntreAyB a b 
    | a > b = error "b debe ser mayor que a"
    | a == b = a 
    | a < b = b + sumaEntreAyB a (b-1);

sumaEntreAyBFuncion::Int->Int->(Int->Int)->Int
sumaEntreAyBFuncion a b f
    | a > b = error "b debe ser mayor que a"
    | a == b = f a 
    | a < b =  (f b) + sumaEntreAyBFuncion a (b-1) f;

sumG::(Int->Int)->Int->Int
sumG f n = sumaEntreAyBFuncion 0 n f;

esDivisor::Int->Int->Bool
esDivisor n1 n2 
    | (modulo n1 n2 == 0) = True
    | otherwise = False;

primerDivisor::Int->Int
primerDivisor n = primerDivisorAux n 2;

{-Pre: n1 empieza en 2-}

primerDivisorAux::Int->Int->Int
primerDivisorAux n n1 
    | esDivisor n n1 = n1
    | otherwise = primerDivisorAux n (n1+1);
        
esPrimo::Int->Bool
esPrimo n 
    | (primerDivisor n == n) = True
    | otherwise  = False;


