module TwistAST where

data QTy = Qubit | Ent QTy QTy
    deriving (Show, Ord, Eq)

data StTy = Pure | Mixed
    deriving (Show, Ord, Eq)

data TwTy = TwBool | QuantTy StTy QTy | Prod TwTy TwTy | Func TwTy TwTy
    deriving (Show, Ord, Eq)

type Label = String

data TwEx = Var String (Maybe TwTy) Label | VarNull (Maybe TwTy) Label
        | App TwEx TwEx (Maybe TwTy) Label | Pair TwEx TwEx (Maybe TwTy) Label
        | QRef String (Maybe TwTy) Label | QPair TwEx TwEx (Maybe TwTy) Label
        | LetEx TwEx TwEx TwEx (Maybe TwTy) Label
        | ITE TwEx TwEx TwEx (Maybe TwTy) Label
        | TwT (Maybe TwTy) Label | TwF (Maybe TwTy) Label
        | QInit (Maybe TwTy) Label
        | U1 String TwEx (Maybe TwTy) Label | U2 String TwEx (Maybe TwTy) Label
        | Msr TwEx (Maybe TwTy) Label
        | MkEnt StTy TwEx (Maybe TwTy) Label
        | Split StTy TwEx (Maybe TwTy) Label
        | Cast StTy TwEx (Maybe TwTy) Label
    deriving (Show, Ord, Eq)

data TwProg = Fun String TwEx TwEx TwProg (Maybe TwTy) Label
        | Main TwEx (Maybe TwTy) Label
    deriving (Show, Ord, Eq)
