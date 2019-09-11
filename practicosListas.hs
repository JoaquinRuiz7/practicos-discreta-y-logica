{--Practico de listas --}
suma::[Int]->Int
suma [] = 0;
suma (x:xs) = x + suma xs;

largoLista::[a]->Int
largoLista [] = 0;
largoLista (x:xs) = 1 + largoLista xs;

multiplicarElementosDeLalista::[Int]->Int
multiplicarElementosDeLalista [] = 1;
multiplicarElementosDeLalista (x:xs) = x * multiplicarElementosDeLalista xs;

sonTodosTrue::[Bool]->Bool
sonTodosTrue [] = True;
sonTodosTrue (x:xs) = x && sonTodosTrue xs;

hayAlMenosUnTrue::[Bool]->Bool
hayAlMenosUnTrue [] = False;
hayAlMenosUnTrue (x:xs) = x || hayAlMenosUnTrue xs;