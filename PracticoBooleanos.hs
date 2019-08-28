module PracticoBooleanos where 
import Prelude (Show)

data Bool = True|False
    deriving (Show)
not::Bool->Bool
not b = case b of {
    True->False;
    False->True;
}

and ::Bool->Bool->Bool
and True False = False
and False True = False
and True True = True
and False False = False;
and3::Bool->Bool->Bool
and3 True True = True;
and3 _ _ = False;

and2::Bool->Bool->Bool
and2 an1 an2 = case an1 of{
    True -> case an2 of {
        True->True;
        False->False;
    };
    False->False;
}

or::Bool->Bool->Bool
or True _ = True
or False b2 = b2;

or2::Bool->Bool->Bool
or2 b1 b2 = case b1 of {
    True->True;
    False ->b2;
}
entonces::Bool->Bool->Bool
entonces False _ = True
entonces True b2 = b2

         
entonces2::Bool->Bool->Bool
entonces2 e1 e2 = case e1 of {
    True->case e2 of {
        True->True;
        False->False;
    };
    False->True;
}
sii::Bool->Bool->Bool
sii True b2 = b2
sii False False = True;



sii2::Bool->Bool->Bool
sii2 sii1 sii2 = case sii1 of {
    True-> case sii2 of {
        True->True;
        False->False;
    };
    False-> case sii2 of{
        False->True;
        True->False;
    };
}

--Ejercicio 3 
andConOrYNot::Bool->Bool->Bool
andConOrYNot b1 b2 = not ( or (not b1 ) (not b2)); 

orConAndYNot::Bool->Bool->Bool
orConAndYNot b1 b2 = not ( and (not b1 ) (not b2 ));
orConNotYEntonces::Bool->Bool->Bool
orConNotYEntonces b1 b2 = entonces (not b1) b2;
andConEntoncesYNot::Bool->Bool->Bool
andConEntoncesYNot b1 b2 = not (entonces b1 (not b2 ));
siiEntoncesNot::Bool->Bool->Bool
siiEntoncesNot b1 b2  =  andConEntoncesYNot (entonces b1 b2 ) (entonces b2 b1);