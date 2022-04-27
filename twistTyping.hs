import qualified Data.Set as Set
import qualified Data.Map as Map
import Language.Haskell.TH (Exp(LetE))

import TwistAST

data TyEx = PlainVar TyVar | ExactlySt StTy | ExactlyQTy QTy | Exactly TwTy |  ExactlyQ StTy QTy  | QEx TyEx TyEx | EntEx TyEx TyEx | FuncEx TyEx TyEx | ProdEx TyEx TyEx
    deriving (Show, Ord, Eq)

type TyVar = String

type TyCons = (TyEx, TyEx)

data TwExL =
            QInitL (Maybe TwTy) String | VarL String (Maybe TwTy)  String  | FunVarL String (Maybe TwTy) String
            | U1L TwExL (Maybe TwTy)  String | U2L TwExL (Maybe TwTy) String
            | LetExL TwExL TwExL TwExL(Maybe TwTy)  String | AppL TwExL TwExL (Maybe TwTy)  String | PairL TwExL TwExL (Maybe TwTy) String | QRefL String (Maybe TwTy) String | QPairL  TwExL TwExL (Maybe TwTy) String
            | ITEL TwExL TwExL TwExL (Maybe TwTy) String | TwTL (Maybe TwTy)  String | TwFL (Maybe TwTy)  String | MsrL TwExL (Maybe TwTy) String
            | MkEntL StTy TwExL (Maybe TwTy) String  | SplitL StTy TwExL (Maybe TwTy)  String | CastL StTy TwExL (Maybe TwTy) String
    deriving (Show, Ord, Eq)

-- (foldl Set.union Set.empty (map (\x -> Set.union (freeVars $ fst x)  (freeVars $ snd x))  (genConstraints expr)))

labeledAST' :: String -> TwEx -> TwExL
labeledAST' rootStr (QInit t) = QInitL t rootStr
labeledAST' rootStr (TwT t) = TwTL t rootStr
labeledAST' rootStr (TwF t) = TwFL t rootStr

labeledAST' rootStr (Var s t) = VarL s t rootStr
labeledAST' rootStr (QRef  nme t) = QRefL  nme t rootStr

labeledAST' rootStr (U1 s expr t) = U1L (labeledAST' (rootStr++"0") expr) t rootStr
labeledAST' rootStr (U2 s expr t) = U2L (labeledAST' (rootStr++"0") expr) t rootStr
labeledAST' rootStr (Msr expr t) = MsrL (labeledAST' (rootStr++"0") expr) t rootStr

labeledAST' rootStr (MkEnt stty expr t) = MkEntL stty (labeledAST' (rootStr++"0") expr) t rootStr
labeledAST' rootStr (Split stty expr t) = SplitL stty (labeledAST' (rootStr++"0") expr) t rootStr
labeledAST' rootStr (Cast stty expr t) = CastL stty (labeledAST' (rootStr++"0") expr) t rootStr

labeledAST' rootStr (App e1 e2 t) = AppL (labeledAST' (rootStr++"0") e1) (labeledAST' (rootStr++"1") e2) t rootStr
labeledAST' rootStr (Pair e1 e2 t) = PairL (labeledAST' (rootStr++"0") e1) (labeledAST' (rootStr++"1") e2) t rootStr
labeledAST' rootStr (QPair e1 e2 t) = QPairL  (labeledAST' (rootStr++"0") e1) (labeledAST' (rootStr++"1") e2) t rootStr


labeledAST' rootStr (LetEx e1 e2 e3 t) = LetExL (labeledAST' (rootStr++"0") e1)  (labeledAST' (rootStr++"1") e2) (labeledAST' (rootStr++"2") e3) t rootStr
labeledAST' rootStr (ITE e1 e2 e3 t) = ITEL (labeledAST' (rootStr++"0") e1)  (labeledAST' (rootStr++"1") e2) (labeledAST' (rootStr++"2") e3) t rootStr

labeledAST :: TwEx -> TwExL
labeledAST = labeledAST' "0"


getLabel :: TwExL -> String
getLabel (QInitL _ s) = s
getLabel (TwTL _ s) = s
getLabel (TwFL _ s) = s
getLabel (VarL _ _ s) = s
getLabel (QRefL __ _ s) = s
getLabel (FunVarL _ _ s) = s
getLabel (U1L expr t s) = s
getLabel (U2L expr t s) = s
getLabel (MsrL expr t s) =s

getLabel (MkEntL stty expr t s) = s
getLabel (SplitL stty expr t s) = s
getLabel (CastL stty expr t s) = s

getLabel (AppL e1 e2 t s) = s
getLabel (PairL e1 e2 t s ) = s
getLabel (QPairL e1 e2 t s ) = s


getLabel (LetExL e1 e2 e3 t s) = s
getLabel (ITEL e1 e2 e3 t s) = s

ctxConstraints :: TwExL -> Map.Map TwExL TyVar -> [TyCons]
ctxConstraints exp map = case Map.lookup exp map of
  Nothing -> []
  Just s -> [(PlainVar $ getLabel exp, PlainVar s)]

genConstraints' ::  TwExL -> Map.Map TwExL TyVar -> [TyCons]
genConstraints' exp@(QInitL _ label) ctx = (PlainVar label, ExactlyQ Pure Qubit) : ctxConstraints exp ctx
genConstraints' exp@(U1L input _ label) ctx = (PlainVar label, PlainVar (getLabel input)): genConstraints'  input ctx ++ ctxConstraints exp ctx
genConstraints' exp@(U2L input _ label) ctx = (PlainVar label, PlainVar (getLabel input)): genConstraints' input ctx ++ ctxConstraints exp ctx
genConstraints' exp@(QPairL l r _ label) ctx = (PlainVar label, QEx (PlainVar ("st" ++ lty)) (EntEx (PlainVar ("q" ++ lty)) (PlainVar ("q" ++ lty)))) : (PlainVar lty, QEx (PlainVar ("st" ++ lty)) (PlainVar ("q" ++ lty))): (PlainVar lty, PlainVar rty): genConstraints' l ctx ++ genConstraints' r ctx ++ ctxConstraints exp ctx
    where (lty, rty) = tMap getLabel (l, r)
genConstraints' exp@(LetExL lhs rhs expr _ label) ctx = (PlainVar (getLabel lhs), PlainVar (getLabel rhs)):genConstraints' lhs ctx  ++ genConstraints' rhs ctx ++ genConstraints' expr (Map.insert lhs (getLabel lhs) ctx)
genConstraints' exp@(SplitL sty expr  _ label) ctx = [(PlainVar (getLabel expr),  QEx (PlainVar ("st" ++ exty)) (PlainVar ("q" ++ exty)))]
    where exty = getLabel expr


genConstraints :: TwEx -> [TyCons]
genConstraints = flip genConstraints' Map.empty . labeledAST

solveConstraints :: [TyCons] -> Maybe (Map.Map TyEx TyEx)
solveConstraints [] = Just Map.empty
solveConstraints (eq : rest) =
    case eq of
        (lhs, rhs) | lhs == rhs -> solveConstraints rest
        (PlainVar x, rhs) | not $ Set.member x (freeVars rhs) ->  Map.union <$> Just (Map.singleton (PlainVar x) rhs) <*> solveConstraints (map (tMap (subst (PlainVar x) rhs)) rest)
        (lhs, PlainVar y) | not $ Set.member y (freeVars lhs) ->  Map.union <$> Just (Map.singleton (PlainVar y) lhs) <*> solveConstraints (map (tMap (subst (PlainVar y) lhs)) rest)
        (FuncEx t1 t2, FuncEx s1 s2) -> solveConstraints ((t1, s1):(t2,s2):rest)
        (ProdEx t1 t2, ProdEx s1 s2) -> solveConstraints ((t1, s1):(t2,s2):rest)
        (QEx t1 t2, QEx s1 s2) -> solveConstraints ((t1, s1):(t2,s2):rest)
        (QEx t1 t2, ExactlyQ s1 s2) -> solveConstraints ((t1, ExactlySt s1):(t2,ExactlyQTy s2):rest)
        (EntEx t1 t2, EntEx s1 s2) -> solveConstraints ((t1, s1):(t2,s2):rest)
        (lhs, rhs) -> Nothing


freeVars :: TyEx -> Set.Set TyVar
freeVars (PlainVar var) = Set.singleton var
freeVars (Exactly _) = Set.empty
freeVars (ExactlyQ _ _) = Set.empty
freeVars (ExactlySt _) = Set.empty
freeVars (ExactlyQTy _) = Set.empty
freeVars (FuncEx left right) = Set.union (freeVars left) (freeVars right)
freeVars (ProdEx left right) = Set.union (freeVars left) (freeVars right)
freeVars (QEx left right) = Set.union (freeVars left) (freeVars right)
freeVars (EntEx left right) = Set.union (freeVars left) (freeVars right)

-- freeVars (QTy (EntEx left right)) = Set.union (freeVars left) (freeVars right)

subst :: TyEx -> TyEx -> TyEx -> TyEx
subst before after expr | expr == before = after
subst before after (FuncEx ty1 ty2) = FuncEx (subst before after ty1) (subst before after ty2)
subst before after (ProdEx ty1 ty2) = ProdEx (subst before after ty1) (subst before after ty2)
subst before after (QEx ty1 ty2) = QEx (subst before after ty1) (subst before after ty2)
subst before after (EntEx ty1 ty2) = EntEx (subst before after ty1) (subst before after ty2)
subst  _ _ expr = expr

tMap :: (a->b) -> (a,a) -> (b,b)
tMap f (x,y) = (f x, f y)

freshVars :: Int -> Set.Set TyVar -> [TyVar]
freshVars n tyVars = take n [s | s <- allStringsLetters, not (Set.member s strs)]
    where
        strs = Set.union (Set.singleton "") tyVars
        allStringsLetters = [c : s | s <- "" : allStringsLetters, c <- ['a'..'z']]
