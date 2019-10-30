module Arboles where 
data BinTree a = Empty|Node a (BinTree a) (BinTree a)
    deriving (Show)
{--
   a
b        NULL
NULL NULL
Programar este arbol
arbol::Bin
arbol = Nodo 'a' (Nodo 'b' (NULL) (NULL) ) (NULL)
--}
--Cantidad de nodos
cantidadNodos::BinTree a->Int
cantidadNodos Empty = 0
cantidadNodos (Node a t1 t2) = 1+cantidadNodos t1+cantidadNodos t2;
--Suma de todos los nodos de un binTree de tipo int
sumaNodos::BinTree Int->Int
sumaNodos Empty = 0
sumaNodos (Node n t1 t2) = n+sumaNodos t1+sumaNodos t2;
--Ej 3 Dado un arbol y un predicado 
-- a retornar si todos los nodos cumplen el predicado
todosNodosCumplen::BinTree a ->(a->Bool)->Bool
todosNodosCumplen Empty p = True
todosNodosCumplen (Node a t1 t2) p = (p a) && todosNodosCumplen t1 p && todosNodosCumplen t2 p;
-- b retornar si existe un nodo que cumpla el predicado
algunoQueCumpla::BinTree a-> (a->Bool)->Bool
algunoQueCumpla Empty p = False
algunoQueCumpla (Node a t1 t2) p = (p a) || algunoQueCumpla t1 p || algunoQueCumpla t2 p;
arbolPrueba::BinTree Int
arbolPrueba = Node 1 (Node 2 (Empty) (Empty)) (Node 3 (Empty) (Empty));
--Ej 4 linealice un arbol en pasar a lista
--a PreOrden -> Medio izquierdo derecho -> [1,2,3]\
aListaPreOrden::BinTree a ->[a]
aListaPreOrden Empty = []
aListaPreOrden (Node a t1 t2 ) = a:[]++aListaPreOrden t1 ++ aListaPreOrden t2;
-- idem pero inorden izquierda medio derecha 2,1,3
aListaInorden::BinTree a ->[a]
aListaInorden Empty = []
aListaInorden ( Node a t1 t2 ) = aListaInorden t1 ++ a:[] ++ aListaInorden t2;
-- idem pero izq der medio 2,3,1
aListaOrden::BinTree a -> [a]
aListaOrden Empty = []
aListaOrden (Node a t1 t2 ) = aListaOrden t1 ++ aListaOrden t2 ++ a:[];