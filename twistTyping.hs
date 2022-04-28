import qualified Data.Set as Set
import qualified Data.Map as Map
import Language.Haskell.TH (Exp(LetE))

import TwistAST
import TwistParsing
import Data.Maybe (fromJust)


data TyEx = PlainVar TyVar | ExactlySt StTy | ExactlyQTy QTy | Exactly TwTy |  ExactlyQ StTy QTy  | QEx TyEx TyEx | EntEx TyEx TyEx | FuncEx TyEx TyEx | ProdEx TyEx TyEx
    deriving (Show, Ord, Eq)

type TyVar = String

type TyCons = (TyEx, TyEx)

data TwExL =
            QInitL (Maybe TwTy) String | VarL String (Maybe TwTy)  String  | VarNullL (Maybe TwTy) String | FunVarL String (Maybe TwTy) String
            | U1L String TwExL (Maybe TwTy)  String | U2L String TwExL (Maybe TwTy) String
            | LetExL TwExL TwExL TwExL(Maybe TwTy)  String | AppL TwExL TwExL (Maybe TwTy)  String | PairL TwExL TwExL (Maybe TwTy) String | QRefL String (Maybe TwTy) String | QPairL  TwExL TwExL (Maybe TwTy) String
            | ITEL TwExL TwExL TwExL (Maybe TwTy) String | TwTL (Maybe TwTy)  String | TwFL (Maybe TwTy)  String | MsrL TwExL (Maybe TwTy) String
            | MkEntL StTy TwExL (Maybe TwTy) String  | SplitL StTy TwExL (Maybe TwTy)  String | CastL StTy TwExL (Maybe TwTy) String
    deriving (Show, Ord, Eq)

-- (foldl Set.union Set.empty (map (\x -> Set.union (freeVars $ fst x)  (freeVars $ snd x))  (genConstraints expr)))

labeledAST' :: String -> TwEx -> TwExL
labeledAST' rootStr (QInit t) = QInitL t rootStr
labeledAST' rootStr (VarNull t) = VarNullL  t rootStr
labeledAST' rootStr (TwT t) = TwTL t rootStr
labeledAST' rootStr (TwF t) = TwFL t rootStr

labeledAST' rootStr (Var s t) = VarL s t rootStr
labeledAST' rootStr (QRef  nme t) = QRefL  nme t rootStr

labeledAST' rootStr (U1 s expr t) = U1L s (labeledAST' (rootStr++"0") expr) t rootStr
labeledAST' rootStr (U2 s expr t) = U2L s (labeledAST' (rootStr++"0") expr) t rootStr
labeledAST' rootStr (Msr expr t) = MsrL (labeledAST' (rootStr++"0") expr) t rootStr

labeledAST' rootStr (MkEnt stty expr t) = MkEntL stty (labeledAST' (rootStr++"0") expr) t rootStr
labeledAST' rootStr (Split stty expr t) = SplitL stty (labeledAST' (rootStr++"0") expr) t rootStr
labeledAST' rootStr (Cast stty expr t) = CastL stty (labeledAST' (rootStr++"0") expr) t rootStr

labeledAST' rootStr (App e1 e2 t) = AppL (labeledAST' (rootStr++"0") e1) (labeledAST' (rootStr++"1") e2) t rootStr
labeledAST' rootStr (Pair e1 e2 t) = PairL (labeledAST' (rootStr++"0") e1) (labeledAST' (rootStr++"1") e2) t rootStr
labeledAST' rootStr (QPair e1 e2 t) = QPairL  (labeledAST' (rootStr++"0") e1) (labeledAST' (rootStr++"1") e2) t rootStr


labeledAST' rootStr (LetEx e1 e2 e3 t) = LetExL (labeledAST' (rootStr++"0") e1)  (labeledAST' (rootStr++"1") e2) (labeledAST' (rootStr++"2") e3) t rootStr
labeledAST' rootStr (ITE e1 e2 e3 t) = ITEL (labeledAST' (rootStr++"0") e1)  (labeledAST' (rootStr++"1") e2) (labeledAST' (rootStr++"2") e3) t rootStr

labeledAST :: String -> TwEx -> TwExL
labeledAST prefix = labeledAST' $ prefix++"0"


getLabel :: TwExL -> String
getLabel (VarNullL _ s) = s
getLabel (QInitL _ s) = s
getLabel (TwTL _ s) = s
getLabel (TwFL _ s) = s
getLabel (VarL _ _ s) = s
getLabel (QRefL __ _ s) = s
getLabel (FunVarL _ _ s) = s
getLabel (U1L _ expr t s) = s
getLabel (U2L _ expr t s) = s
getLabel (MsrL expr t s) =s

getLabel (MkEntL stty expr t s) = s
getLabel (SplitL stty expr t s) = s
getLabel (CastL stty expr t s) = s

getLabel (AppL e1 e2 t s) = s
getLabel (PairL e1 e2 t s ) = s
getLabel (QPairL e1 e2 t s ) = s


getLabel (LetExL e1 e2 e3 t s) = s
getLabel (ITEL e1 e2 e3 t s) = s

getType :: TwEx -> Maybe TwTy
getType (VarNull t) = t
getType (QInit t) = t
getType (TwT t) = t
getType (TwF  t) = t
getType (Var  _ t) = t
getType (QRef _  t) = t

getType (U1 _ expr t ) = t
getType (U2 _ expr t ) = t
getType (Msr expr t) = t

getType (MkEnt stty expr t) = t
getType (Split stty expr t) = t
getType (Cast stty expr t) = t

getType (App e1 e2 t) = t
getType (Pair e1 e2 t) = t
getType (QPair e1 e2 t) = t


getType (LetEx e1 e2 e3 t) = t
getType (ITE e1 e2 e3 t) = t

clearType :: TwEx -> TwEx
clearType (VarNull t) = VarNull Nothing
clearType (QInit t) = QInit Nothing
clearType (TwT t) = TwT Nothing
clearType (TwF  t) = TwF Nothing
clearType (Var  str t) = Var str Nothing
clearType (QRef str t) = QRef str Nothing

clearType (U1 str expr t ) = U1 str expr Nothing
clearType (U2 str expr t ) =  U2 str expr Nothing
clearType (Msr expr t) = (Msr expr Nothing)

clearType (MkEnt stty expr t) = (MkEnt stty expr Nothing)
clearType (Split stty expr t) = (Split stty expr Nothing)
clearType (Cast stty expr t) = (Cast stty expr Nothing)

clearType (App e1 e2 t) = (App e1 e2 Nothing)
clearType (Pair e1 e2 t) = (Pair e1 e2 Nothing)
clearType (QPair e1 e2 t) = (QPair e1 e2 Nothing)


clearType (LetEx e1 e2 e3 t) = (LetEx e1 e2 e3 Nothing)
clearType (ITE e1 e2 e3 t) = (ITE e1 e2 e3 Nothing)



unlabel :: TwExL -> TwEx
unlabel (VarNullL x _) = VarNull x
unlabel (QInitL x _) = QInit  x
unlabel (TwTL x _) = TwT x
unlabel (TwFL x _) = TwF x
unlabel (VarL x y  _ ) = Var x y 
unlabel (QRefL x y s) = QRef x y 
unlabel (FunVarL x y s) = error "not in TwEx"
unlabel (U1L nme expr t s) = U1 nme (unlabel expr) t
unlabel (U2L nme expr t s) = U2 nme (unlabel expr) t
unlabel (MsrL expr t s) = Msr (unlabel expr) t

unlabel (MkEntL stty expr t s) = MkEnt stty (unlabel expr) t
unlabel (SplitL stty expr t s) =  Split stty (unlabel expr) t
unlabel (CastL stty expr t s) = Cast stty (unlabel expr) t

unlabel (AppL e1 e2 t s) = App (unlabel e1) (unlabel e2) t
unlabel (PairL e1 e2 t s ) = Pair (unlabel e1) (unlabel e2) t
unlabel (QPairL e1 e2 t s ) = QPair (unlabel e1) (unlabel e2) t


unlabel (LetExL e1 e2 e3 t s) = LetEx (unlabel e1) (unlabel e2) (unlabel e3) t
unlabel (ITEL e1 e2 e3 t s) = ITE (unlabel e1) (unlabel e2) (unlabel e3) t

ctxConstraints :: TwExL -> Map.Map TwEx TyVar -> [TyCons]
ctxConstraints exp map = case Map.lookup (unlabel exp) map of
  Nothing -> []
  Just s -> [(PlainVar $ getLabel exp, PlainVar s)]


genConstraints' ::  TwExL -> Map.Map TwEx TyVar -> [TyCons]

-- Basic data types
genConstraints' exp@ VarL {} ctx = ctxConstraints exp ctx
genConstraints' exp@VarNullL {} ctx = ctxConstraints exp ctx
genConstraints' exp@FunVarL {} ctx = ctxConstraints exp ctx
genConstraints' exp@(TwTL _ label) ctx = (PlainVar label, Exactly TwBool ) : ctxConstraints exp ctx
genConstraints' exp@(TwFL _ label) ctx = (PlainVar label, Exactly TwBool ) : ctxConstraints exp ctx
genConstraints' exp@(QInitL _ label) ctx = (PlainVar label, ExactlyQ Pure Qubit) : ctxConstraints exp ctx
genConstraints' exp@(QRefL _ _ label) ctx = (PlainVar label, QEx (PlainVar $ "st" ++ label) (ExactlyQTy Qubit)) : ctxConstraints exp ctx
genConstraints' exp@(PairL l r _ label) ctx = (PlainVar label, ProdEx (PlainVar (getLabel l )) (PlainVar (getLabel r))):genConstraints' l ctx  ++ genConstraints' r ctx ++ ctxConstraints exp ctx
genConstraints' exp@(QPairL l r _ label) ctx = (PlainVar label, QEx (PlainVar ("st" ++ lty)) (EntEx (PlainVar ("q" ++ lty)) (PlainVar ("q" ++ lty)))) : (PlainVar lty, QEx (PlainVar ("st" ++ lty)) (PlainVar ("q" ++ lty))): (PlainVar lty, PlainVar rty): genConstraints' l ctx ++ genConstraints' r ctx ++ ctxConstraints exp ctx
    where (lty, rty) = tMap getLabel (l, r)

-- Operations
genConstraints' exp@(AppL fun input _ label) ctx = (PlainVar $ getLabel fun, FuncEx (PlainVar $ getLabel input) $ PlainVar label):genConstraints' fun ctx  ++ genConstraints' input ctx ++ ctxConstraints exp ctx
genConstraints' exp@(U1L _ input _ label) ctx = (PlainVar label, PlainVar (getLabel input)): genConstraints'  input ctx ++ ctxConstraints exp ctx
genConstraints' exp@(U2L _ input _ label) ctx = (PlainVar label, PlainVar (getLabel input)): genConstraints' input ctx ++ ctxConstraints exp ctx
genConstraints' exp@(MsrL input _ label) ctx = (PlainVar label, Exactly TwBool):(PlainVar (getLabel input), QEx (PlainVar $ "st"++getLabel input) (ExactlyQTy Qubit)): genConstraints' input ctx ++ ctxConstraints exp ctx

-- Control-flow
genConstraints' exp@(ITEL cond left right _ label) ctx = (PlainVar (getLabel cond), Exactly TwBool):(PlainVar (getLabel right), PlainVar (getLabel left)):(PlainVar $ getLabel left, QEx (PlainVar $"st"++getLabel left) (PlainVar $"q"++getLabel left)):(PlainVar label, QEx (ExactlySt Mixed) (PlainVar $ "q"++getLabel left) ): genConstraints' left ctx ++ genConstraints' right ctx ++ ctxConstraints exp ctx
genConstraints' exp@(LetExL lhs@(PairL l1 l2 _ llbl) rhs expr _ label) ctx = (PlainVar label, PlainVar (getLabel expr)):(PlainVar (getLabel lhs), PlainVar (getLabel rhs)):genConstraints' lhs ctx  ++ genConstraints' rhs ctx ++ genConstraints' expr (Map.insert (unlabel l1) (getLabel l1) ctx')
    where ctx' = Map.insert (unlabel l2) (getLabel l2) ctx

--  Entangle, Split, and Cast: the Purity operators
genConstraints' exp@(SplitL sty input  _ label) ctx = (PlainVar (getLabel input),  QEx (PlainVar ("st" ++ exty)) (EntEx (PlainVar ("q" ++ exty ++ "0")) (PlainVar ("q" ++ exty ++ "1")))) : (PlainVar label, ProdEx (QEx (PlainVar ("st"++exty)) (PlainVar ("q" ++ exty ++ "0"))) (QEx (PlainVar ("st"++exty)) (PlainVar ("q" ++ exty ++ "1")))) : genConstraints' input ctx ++ ctxConstraints exp ctx
    where exty = getLabel input
genConstraints' exp@(MkEntL sty input  _ label) ctx = (PlainVar label,  QEx (PlainVar ("st" ++ exty)) (EntEx (PlainVar ("ql" ++ exty)) (PlainVar ("qr" ++ exty)))) :
                                                     (PlainVar (getLabel input), ProdEx (QEx (PlainVar ("st"++exty)) (PlainVar ("q" ++ exty ++ "0"))) (QEx (PlainVar ("st"++exty)) (PlainVar ("q" ++ exty ++ "1")))) : genConstraints' input ctx ++ ctxConstraints exp ctx
    where exty = getLabel input
genConstraints' exp@(CastL sty input _ label) ctx = (PlainVar label, QEx (ExactlySt sty) (PlainVar ("q" ++ exty))):(PlainVar exty, QEx (PlainVar ("st"++exty)) (PlainVar ("q" ++ exty))):genConstraints' input ctx ++ ctxConstraints exp ctx
    where exty = getLabel input
genConstraints' _ _ = error "genConstraints error: not implemented"

genConstraintsEx :: String -> Map.Map TwEx TyVar -> TwEx -> [TyCons]
genConstraintsEx prefix map expr = genConstraints' (labeledAST prefix expr) map

progToFunList :: TwProg -> [(String, TwEx, TwEx)] 
progToFunList (Main expr _) = [("Main", VarNull Nothing, expr)]
progToFunList (Fun name input def rest  _) = (name, input, def):progToFunList rest

genConstraintsFunList :: Map.Map TwEx TyVar -> [(String, TwEx, TwEx)] -> [TyCons]
genConstraintsFunList map [("Main", VarNull Nothing, expr)] =  genConstraintsEx "main" map expr
genConstraintsFunList map ((name, input, def):xs) = (PlainVar name, FuncEx (PlainVar (name++"_in")) (PlainVar (name++"0"))):inputConstraint ++ genConstraintsEx name (Map.insert (clearType input) (name++"_in") map) def ++ genConstraintsFunList (Map.insert (Var name Nothing) name map ) xs 
    where inputConstraint =
                case getType input of
                    Nothing -> []
                    Just t -> [(PlainVar $ name++"_in", Exactly t)]

genConstraintsProg :: TwProg -> [TyCons]
genConstraintsProg prog = genConstraintsFunList Map.empty (progToFunList prog)

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
        (ExactlyQ t1 t2, QEx s1 s2) -> solveConstraints ((ExactlySt t1, s1):(ExactlyQTy t2, s2):rest)
        (Exactly (QuantTy t1 t2), QEx s1 s2) -> solveConstraints ((ExactlySt t1, s1):(ExactlyQTy t2, s2):rest)
        (QEx s1 s2, Exactly (QuantTy t1 t2)) -> solveConstraints ((ExactlySt t1, s1):(ExactlyQTy t2, s2):rest)
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


ex = LetEx (Var "x" Nothing) (QInit  Nothing) (ITE (Msr (QInit Nothing) Nothing) (Var "x" Nothing) (U2 "X" (Var "x" Nothing) Nothing) Nothing ) Nothing