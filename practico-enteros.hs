module PracticoEnteros where 

sumatoria::Int->Int
sumatoria n 
        | n == 0 = 0
        | n > 0 = n + sumatoria (n-1);

factorial::Int->Int
factorial n 
        | n == 0 = 1
        | n > 0 = n* factorial(n-1);
dosALaN::Int->Int
dosALaN n 
        | n == 0 = 1
        | n > 0 = 2 * dosALaN(n-1);
exponencial::Int->Int->Int
exponencial n n2 
            | n==0 && n2 == 0 = error "Math error"
            | n == 0 = 0
            | n2 == 0 = 1
            | otherwise = n * exponencial n (n2-1);
copias::String->Int->String
copias s n
        | n<0 = error "Enter a positive number"
        | n == 1 = s
        | otherwise =  s ++ copias s (n-1);
productoNatConSuma::Int->Int->Int
productoNatConSuma n1 n2
                        | n2 == 0 = 0
                        | n2 > 0 = n1 + productoNatConSuma n1 (n2-1);
cocienteConResta::Int->Int->Int
cocienteConResta n1 n2 
                        | n2 == 0 = error "Can´t divide by 0"
                        | n1 == 0 = 0 
                        | (n1 > 0) && (n2 > n1) = 0
                        | n2 > 0 = 1 + cocienteConResta (n1-n2) n2;
modulo::Int->Int->Int
modulo n1 n2
            | n2 == 0 = error "Undefined"
            | n1 == 0 = 0
            | (n1 > 0) && (n2>n1) = n1
            | n2 > 0 = modulo (n1-n2) n2;

esDivisor::Int->Int->Bool
esDivisor n n2 
        | modulo n n2 == 0 = True
        | otherwise = False;

primerDivisor::Int->Int
primerDivisor n = primerDivisorAux n 2;
{-Pre: n1 empieza en 2-}
primerDivisorAux::Int->Int->Int
primerDivisorAux n n1 
        | esDivisor n n1 = n1
        | otherwise = primerDivisorAux n (n1+1);
            