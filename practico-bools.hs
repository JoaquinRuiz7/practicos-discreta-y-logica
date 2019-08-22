module PracticoBools where

nott::Bool->Bool
nott b
    | ( b == False ) = True
    | otherwise = False;

andd::Bool->Bool->Bool
andd b1 b2 
        | ( b1 && b2 ) = True
        | otherwise = False;

orr::Bool->Bool->Bool
orr b1 b2 = b1 || b2;

implica::Bool->Bool->Bool
implica b1 b2 
            | nott b1 = True
            | b1 && nott (b2) = False
            | b1 && b2 = True;

xorr::Bool->Bool->Bool
xorr b1 b2 
    | b1 && (nott b2) = True
    | ( nott b1 ) && b2 = True
    | otherwise = False;

sii::Bool->Bool->Bool
sii b1 b2 
        | b1 && b2 = True
        | b1 && (nott b2) = False
        | (nott b1) && b2 = False
        | (not b1) && (not b2) = True;

            
    