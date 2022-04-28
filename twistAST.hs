module TwistAST where

data Annotation = Annotation (Maybe TwTy) String
    deriving (Show, Ord, Eq)

data QTy = Qubit | Ent QTy QTy
    deriving (Show, Ord, Eq)

data StTy = Pure | Mixed
    deriving (Show, Ord, Eq)

data TwTy = TwBool | QuantTy StTy QTy | Prod TwTy TwTy | Func TwTy TwTy
    deriving (Show, Ord, Eq)

data TwEx =
            QInit Annotation | Var String Annotation | VarNull Annotation
            | U1 String TwEx Annotation | U2 String TwEx Annotation
            | LetEx TwEx TwEx TwEx Annotation | App TwEx TwEx Annotation | Pair TwEx TwEx Annotation | QRef  String Annotation | QPair  TwEx TwEx Annotation
            | ITE TwEx TwEx TwEx Annotation | TwT Annotation | TwF Annotation | Msr TwEx Annotation
            | MkEnt StTy TwEx Annotation | Split StTy TwEx Annotation | Cast StTy TwEx Annotation
    deriving (Show, Ord, Eq)

data TwProg = Fun String TwEx TwEx TwProg Annotation | Main TwEx Annotation
    deriving (Show, Ord, Eq)
