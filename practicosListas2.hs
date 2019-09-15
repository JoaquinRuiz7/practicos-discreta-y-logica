duplicate::[Int]->[Int]
duplicate [] = []
duplicate (x:xs) = x:x:(duplicate xs);

largoLista::[a]->Int
largoLista [] = 0
largoLista (x:xs) = 1 + largoLista xs;

sumLargos::[[a]]->Int
sumLargos [] = 0
sumLargos (x:xs) = (largoLista x) + sumLargos xs;
