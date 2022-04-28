import qualified Data.Set as Set
import qualified Data.Map as Map
import Language.Haskell.TH (Exp(LetE))

import TwistAST
import Data.Maybe (fromJust)


data TyEx = PlainVar TyVar | ExactlySt StTy | ExactlyQTy QTy | Exactly TwTy | ExactlyQ StTy QTy | QEx TyEx TyEx | EntEx TyEx TyEx | FuncEx TyEx TyEx | ProdEx TyEx TyEx
    deriving (Show, Ord, Eq)

type TyVar = String

type TyCons = (TyEx, TyEx)
-- (foldl Set.union Set.empty (map (\x -> Set.union (freeVars $ fst x) (freeVars $ snd x)) (genConstraints expr)))

getLabel :: TwEx -> Label
getLabel (VarNull _ s) = s
getLabel (QInit _ s) = s
getLabel (TwT _ s) = s
getLabel (TwF _ s) = s
getLabel (Var _ _ s) = s
getLabel (QRef _ _ s) = s
getLabel (U1 _ _ _ s) = s
getLabel (U2 _ _ _ s) = s
getLabel (Msr _ _ s) = s

getLabel (MkEnt _ _ _ s) = s
getLabel (Split _ _ _ s) = s
getLabel (Cast _ _ _ s) = s

getLabel (App _ _ _ s) = s
getLabel (Pair _ _ _ s) = s
getLabel (QPair _ _ _ s) = s

getLabel (LetEx _ _ _ _ s) = s
getLabel (ITE _ _ _ _ s) = s

getType :: TwEx -> Maybe TwTy
getType (VarNull t _) = t
getType (QInit t _) = t
getType (TwT t _) = t
getType (TwF t _) = t
getType (Var _ t _) = t
getType (QRef _ t _) = t

getType (U1 _ _ t _) = t
getType (U2 _ _ t _) = t
getType (Msr _ t _) = t

getType (MkEnt _ _ t _) = t
getType (Split _ _ t _) = t
getType (Cast _ _ t _) = t

getType (App _ _ t _) = t
getType (Pair _ _ t _) = t
getType (QPair _ _ t _) = t

getType (LetEx _ _ _ t _) = t
getType (ITE _ _ _ t _) = t

clearType :: TwEx -> TwEx
clearType (VarNull _ label) = VarNull Nothing label
clearType (QInit _ label) = QInit Nothing label
clearType (TwT _ label) = TwT Nothing label
clearType (TwF _ label) = TwF Nothing label
clearType (Var str _ label) = Var str Nothing label
clearType (QRef str _ label) = QRef str Nothing label

clearType (U1 str expr _ label) = U1 str expr Nothing label
clearType (U2 str expr _ label) = U2 str expr Nothing label
clearType (Msr expr _ label) = Msr expr Nothing label

clearType (MkEnt stty expr _ label) = MkEnt stty expr Nothing label
clearType (Split stty expr _ label) = Split stty expr Nothing label
clearType (Cast stty expr _ label) = Cast stty expr Nothing label

clearType (App e1 e2 _ label) = App e1 e2 Nothing label
clearType (Pair e1 e2 _ label) = Pair e1 e2 Nothing label
clearType (QPair e1 e2 _ label) = QPair e1 e2 Nothing label

clearType (LetEx e1 e2 e3 _ label) = LetEx e1 e2 e3 Nothing label
clearType (ITE e1 e2 e3 _ label) = ITE e1 e2 e3 Nothing label

ctxConstraints :: TwEx -> Map.Map TwEx TyVar -> [TyCons]
ctxConstraints exp map = case Map.lookup exp map of
  Nothing -> []
  Just s -> [(PlainVar $ getLabel exp, PlainVar s)]


genConstraints' :: TwEx -> Map.Map TwEx TyVar -> [TyCons]

-- Basic data types
genConstraints' exp@Var {} ctx = ctxConstraints exp ctx
genConstraints' exp@VarNull {} ctx = ctxConstraints exp ctx
genConstraints' exp@(TwT _ label) ctx = (PlainVar label, Exactly TwBool) : ctxConstraints exp ctx
genConstraints' exp@(TwF _ label) ctx = (PlainVar label, Exactly TwBool) : ctxConstraints exp ctx
genConstraints' exp@(QInit _ label) ctx = (PlainVar label, ExactlyQ Pure Qubit) : ctxConstraints exp ctx
genConstraints' exp@(QRef _ _ label) ctx = (PlainVar label, QEx (PlainVar $ "st" ++ label) (ExactlyQTy Qubit)) : ctxConstraints exp ctx
genConstraints' exp@(Pair l r _ label) ctx = (PlainVar label, ProdEx (PlainVar (getLabel l)) (PlainVar (getLabel r))):genConstraints' l ctx ++ genConstraints' r ctx ++ ctxConstraints exp ctx
genConstraints' exp@(QPair l r _ label) ctx = (PlainVar label, QEx (PlainVar ("st" ++ lty)) (EntEx (PlainVar ("q" ++ lty)) (PlainVar ("q" ++ lty)))) : (PlainVar lty, QEx (PlainVar ("st" ++ lty)) (PlainVar ("q" ++ lty))): (PlainVar lty, PlainVar rty): genConstraints' l ctx ++ genConstraints' r ctx ++ ctxConstraints exp ctx
    where (lty, rty) = tMap getLabel (l, r)

-- Operations
genConstraints' exp@(App fun input _ label) ctx = (PlainVar $ getLabel fun, FuncEx (PlainVar $ getLabel input) $ PlainVar label):genConstraints' fun ctx ++ genConstraints' input ctx ++ ctxConstraints exp ctx
genConstraints' exp@(U1 _ input _ label) ctx = (PlainVar label, PlainVar (getLabel input)): genConstraints' input ctx ++ ctxConstraints exp ctx
genConstraints' exp@(U2 _ input _ label) ctx = (PlainVar label, PlainVar (getLabel input)): genConstraints' input ctx ++ ctxConstraints exp ctx
genConstraints' exp@(Msr input _ label) ctx = (PlainVar label, Exactly TwBool):(PlainVar (getLabel input), QEx (PlainVar $ "st"++getLabel input) (ExactlyQTy Qubit)): genConstraints' input ctx ++ ctxConstraints exp ctx

-- Control-flow
genConstraints' exp@(ITE cond left right _ label) ctx = (PlainVar (getLabel cond), Exactly TwBool):(PlainVar (getLabel right), PlainVar (getLabel left)):(PlainVar $ getLabel left, QEx (PlainVar $ "st"++getLabel left) (PlainVar $ "q"++getLabel left)):(PlainVar label, QEx (ExactlySt Mixed) (PlainVar $ "q"++getLabel left)): genConstraints' left ctx ++ genConstraints' right ctx ++ ctxConstraints exp ctx
genConstraints' exp@(LetEx lhs@(Pair l1 l2 _ llbl) rhs expr _ label) ctx = (PlainVar label, PlainVar (getLabel expr)):(PlainVar (getLabel lhs), PlainVar (getLabel rhs)):genConstraints' lhs ctx ++ genConstraints' rhs ctx ++ genConstraints' expr (Map.insert l1 (getLabel l1) ctx')
    where ctx' = Map.insert l2 (getLabel l2) ctx

-- Entangle, Split, and Cast: the Purity operators
genConstraints' exp@(Split sty input _ label) ctx = (PlainVar (getLabel input), QEx (PlainVar ("st" ++ exty)) (EntEx (PlainVar ("q" ++ exty ++ "0")) (PlainVar ("q" ++ exty ++ "1")))) : (PlainVar label, ProdEx (QEx (PlainVar ("st"++exty)) (PlainVar ("q" ++ exty ++ "0"))) (QEx (PlainVar ("st"++exty)) (PlainVar ("q" ++ exty ++ "1")))) : genConstraints' input ctx ++ ctxConstraints exp ctx
    where exty = getLabel input
genConstraints' exp@(MkEnt sty input _ label) ctx = (PlainVar label, QEx (PlainVar ("st" ++ exty)) (EntEx (PlainVar ("ql" ++ exty)) (PlainVar ("qr" ++ exty)))) :
                                                    (PlainVar (getLabel input), ProdEx (QEx (PlainVar ("st"++exty)) (PlainVar ("q" ++ exty ++ "0"))) (QEx (PlainVar ("st"++exty)) (PlainVar ("q" ++ exty ++ "1")))) : genConstraints' input ctx ++ ctxConstraints exp ctx
    where exty = getLabel input
genConstraints' exp@(Cast sty input _ label) ctx = (PlainVar label, QEx (ExactlySt sty) (PlainVar ("q" ++ exty))):(PlainVar exty, QEx (PlainVar ("st"++exty)) (PlainVar ("q" ++ exty))):genConstraints' input ctx ++ ctxConstraints exp ctx
    where exty = getLabel input
genConstraints' _ _ = error "genConstraints error: not implemented"

genConstraintsEx :: String -> Map.Map TwEx TyVar -> TwEx -> [TyCons]
genConstraintsEx prefix map expr = genConstraints' expr map

-- Maps a program to a list whose entries are
--  * Name
--  * Parameters
--  * Body
--  * Label
progToFunList :: TwProg -> [(String, TwEx, TwEx, Label)]
progToFunList (Main expr _ label) = [("main", VarNull Nothing "0.0", expr, label)]
progToFunList (Fun name input def rest _ label) = (name, input, def, label):progToFunList rest

-- TODO: Is the label correct in (Var name Nothing label)? Or does it not matter?
genConstraintsFunList :: Map.Map TwEx TyVar -> [(String, TwEx, TwEx, Label)] -> [TyCons]
genConstraintsFunList map [("main", VarNull Nothing _, expr, _)] = genConstraintsEx "main" map expr
genConstraintsFunList map ((name, input, def, label):xs) = (PlainVar name, FuncEx (PlainVar (name++"_in")) (PlainVar (name++"0"))):inputConstraint ++ genConstraintsEx name (Map.insert (clearType input) (name++"_in") map) def ++ genConstraintsFunList (Map.insert (Var name Nothing label) name map) xs
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
        (PlainVar x, rhs) | not $ Set.member x (freeVars rhs) -> Map.union <$> Just (Map.singleton (PlainVar x) rhs) <*> solveConstraints (map (tMap (subst (PlainVar x) rhs)) rest)
        (lhs, PlainVar y) | not $ Set.member y (freeVars lhs) -> Map.union <$> Just (Map.singleton (PlainVar y) lhs) <*> solveConstraints (map (tMap (subst (PlainVar y) lhs)) rest)
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
subst _ _ expr = expr

tMap :: (a->b) -> (a,a) -> (b,b)
tMap f (x,y) = (f x, f y)

freshVars :: Int -> Set.Set TyVar -> [TyVar]
freshVars n tyVars = take n [s | s <- allStringsLetters, not (Set.member s strs)]
    where
        strs = Set.union (Set.singleton "") tyVars
        allStringsLetters = [c : s | s <- "" : allStringsLetters, c <- ['a'..'z']]


ex = LetEx (Var "x" Nothing "00") (QInit Nothing "10") (ITE (Msr (QInit Nothing "0020") Nothing "020") (Var "x" Nothing "120") (U2 "X" (Var "x" Nothing "0220") Nothing "220") Nothing "20") Nothing "0"
