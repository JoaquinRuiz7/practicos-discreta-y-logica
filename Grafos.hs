module Grafos where

type Vertice = Int
type Arista = (Vertice,Vertice)
type Grafo = [ (Vertice,[Vertice])]
--1)
{-- Implementar la funcion construirGrafo::Int->Grafo
que dado un int agrega todos los vertices de 1 hasta n
al grafo sin ninguna arista
--}
construirGrafo::Int->Grafo
construirGrafo 0 = []
construirGrafo n = (n,[]):construirGrafo (n-1) 
--2
{--
Programar la funcion agregarArista::Grafo->Vertice->Vertice->Grafo
que dado un grafo y dos vertices v1 y v2, agrego la arista que va desde el vertice a al vertice b en el grafo
--}
grafoPrueba::Grafo
grafoPrueba = [(1,[1,1,2,3]),(2,[1,3]),(3,[1,2]) ,(4,[3]) ];

agregarArista::Grafo->Vertice->Vertice->Grafo
agregarArista [] v1 v2= []
agregarArista ( (v,xs):ys ) v1 v2
    | ( v == v1 ) = ( v , (v2:xs ) ):agregarArista ys v1 v2  
    | otherwise = (v,xs):agregarArista ys v1 v2; 
{--
Implementar vertices vertices::Grafo->[Arista]
que dado un grafo retorna la lista de todas los vertices del grafo.
--}
formarPares::Vertice->[Vertice]->[(Vertice,Vertice)]
formarPares v [] = []
formarPares v (x:xs) = (v,x):formarPares v xs;
vertices::Grafo->[Vertice]
vertices [] = []
vertices ( (vertice,l):ys ) = vertice:vertices ys;
{--
Implementar aristas
aristas::grafo->[Arista]
que dado un grafo retorna todas las aristas
--}
aristas::Grafo->[Arista]
aristas [] = []
aristas ( (vertice,l):ys ) = formarPares vertice l ++ aristas ys;

{-- 5) Implementar gradoEntrada::Grafo->Vertice->Int, que dado un grafo y un vertice, 
retorna el grado de entrada del vertice. --}
contiene::Eq a =>a->[a]->Bool
contiene e [] = False
contiene e (x:xs) 
    | x == e = True
    | otherwise = contiene e xs;
--type Grafo = [ (Vertice,[Vertice])]
gradoEntrada::Grafo->Vertice->Int
gradoEntrada [] v = 0
gradoEntrada ((vertice,l):xs) v
    | contiene v l && vertice /= v = 1+gradoEntrada xs v
    | otherwise = gradoEntrada xs v;
{--
implementar una funcion gradoSalida::Grafo->Vertice->Int
--}
gradoSalida::Grafo->Vertice->Int
gradoSalida [] v = 0
gradoSalida ( (vertice,l):xs) v
    | vertice == v = length l - (cantidadLazos ((vertice,l):[]) v) 
    | otherwise = gradoSalida xs v; 
{--
Implementar una funcion que dado un grafo y un vertice retorne e grado de incidencia 
del vertice
--}
cantidadLazos::Grafo->Vertice->Int
cantidadLazos [] v = 0
cantidadLazos ( (vertice, [] ):xs ) v = 0
cantidadLazos ( (vertice, (y:ys) ):xs ) v
    | vertice == y = 1+cantidadLazos ( (vertice,ys):xs ) v
    | otherwise = cantidadLazos ( (vertice,ys):xs ) v;

gradoIncidencia::Grafo->Vertice->Int
gradoIncidencia [] v = 0
gradoIncidencia g v = ( ( cantidadLazos g v ) * 2 ) +gradoEntrada g v + gradoSalida g v;