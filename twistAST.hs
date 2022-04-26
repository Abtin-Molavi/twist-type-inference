module TwistAST where

data QTy = Qubit | Ent QTy QTy
    deriving (Show, Ord, Eq)

data TwTy = TwBool | QuantTy StTy QTy | Prod TwTy TwTy | Func TwTy TwTy
    deriving (Show, Ord, Eq)

data StTy = Pure | Mixed
    deriving (Show, Ord, Eq)

data TwEx =
            QInit (Maybe TwTy) | Var String (Maybe TwTy) | VarNull (Maybe TwTy)
            | U1 String TwEx (Maybe TwTy) | U2 String TwEx (Maybe TwTy)
            | LetEx TwEx TwEx TwEx (Maybe TwTy) | App TwEx TwEx (Maybe TwTy) | Pair TwEx TwEx (Maybe TwTy)
            | ITE TwEx TwEx TwEx (Maybe TwTy)| TwT (Maybe TwTy) | TwF (Maybe TwTy) | Msr TwEx (Maybe TwTy)
            | MkEnt StTy TwEx (Maybe TwTy) | Split StTy TwEx (Maybe TwTy) | Cast StTy TwEx (Maybe TwTy)
    deriving (Show, Ord, Eq)

data TwProg = Fun TwEx TwEx TwEx TwProg (Maybe TwTy) | Main TwEx (Maybe TwTy)
    deriving (Show, Ord, Eq)
