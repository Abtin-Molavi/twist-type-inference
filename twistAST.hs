module TwistAST where

data QTy = Qubit | Ent QTy QTy
    deriving (Show, Ord, Eq)

data StTy = Pure | Mixed
    deriving (Show, Ord, Eq)

data TwTy = TwBool | QuantTy StTy QTy | Prod TwTy TwTy | Func TwTy TwTy
    deriving (Show, Ord, Eq)

data TwEx =
            QInit (Maybe TwTy) String | Var String (Maybe TwTy) String | VarNull (Maybe TwTy) String
            | U1 String TwEx (Maybe TwTy) String | U2 String TwEx (Maybe TwTy) String
            | LetEx TwEx TwEx TwEx (Maybe TwTy) String | App TwEx TwEx (Maybe TwTy) String | Pair TwEx TwEx (Maybe TwTy) String | QRef  String (Maybe TwTy) String | QPair  TwEx TwEx (Maybe TwTy) String
            | ITE TwEx TwEx TwEx (Maybe TwTy) String | TwT (Maybe TwTy) String | TwF (Maybe TwTy) String | Msr TwEx (Maybe TwTy) String
            | MkEnt StTy TwEx (Maybe TwTy) String | Split StTy TwEx (Maybe TwTy) String | Cast StTy TwEx (Maybe TwTy) String
    deriving (Show, Ord, Eq)

data TwProg = Fun String TwEx TwEx TwProg (Maybe TwTy) String | Main TwEx (Maybe TwTy) String
    deriving (Show, Ord, Eq)
