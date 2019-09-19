module Arboles where 
data BinTree a = Empty | Node a (BinTree a) (BinTree a)

size::BinTree a->Int
size Empty = 0
size (Node e (t1) (t2) ) = 1 + size(t1)+size(t2);

sumaNodos::BinTree Int ->Int
sumaNodos Empty = 0
sumaNodos (Node x (t1)(t2)) = x + sumaNodos t1 + sumaNodos t2;

cumplenTodos::BinTree a ->(a->Bool) -> Bool
cumplenTodos Empty p = True
cumplenTodos (Node e (t1) (t2) ) p = (p e) && ( cumplenTodos t1 p ) && ( cumplenTodos t2 p );

algunNodoCumple::BinTree a ->(a->Bool)->Bool
algunNodoCumple Empty p = False
algunNodoCumple (Node e (t1) (t2)) p = p e || ( algunNodoCumple t1 p) || ( algunNodoCumple t2 p);