module PracticoBools where
import Prelude (Show)
data Bool = True|False deriving (Show)
        
not::Bool->Bool
not b = case b of {
    True -> False;
    False->True;
}
not2::Bool->Bool
not2 True = False
not2 _ = True;

and::Bool->Bool->Bool
and True True = True
and _ _ = False;

and2::Bool->Bool->Bool
and2 b1 b2 = case b1 of{
    True -> b2;
    False -> False;
};

or::Bool->Bool->Bool
or True _ = True
or False b2 = b2;

or2::Bool->Bool->Bool
or2 b1 b2 = case b1 of {
    True -> True;
    False -> b2;
};

implica::Bool->Bool->Bool
implica False _ = True
implica True b2 = b2;
implica2::Bool->Bool->Bool
implica2 b1 b2 = case b1 of {
    False -> True;
    True -> b2;
};

xor::Bool->Bool->Bool
xor True False = True
xor False b2 = b2;
xor2::Bool->Bool->Bool
xor2 b1 b2 = case b1 of {
    True-> not b2;
    False -> b2;
};
sii::Bool->Bool->Bool
sii True b2 = b2;
sii False _ = False;

            
    