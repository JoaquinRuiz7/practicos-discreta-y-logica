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
grafoPrueba = [(1,[2,3]),(2,[1]),(3,[1,2])];

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

gradoEntrada::Grafo->Vertice->Int
gradoEntrada g v = undefined