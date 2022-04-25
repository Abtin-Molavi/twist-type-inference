import qualified Data.Set as Set
import qualified Data.Map as Map

data QTy = Qubit | Ent QTy QTy
    deriving (Show, Ord, Eq)

data TwTy = TwBool | QuantTy StTy QTy | Prod TwTy TwTy | Func TwTy TwTy
    deriving (Show, Ord, Eq)

data TyEx = PlainVar TyVar | Exactly TwTy | FuncEx TyEx TyEx | ProdEx TyEx TyEx
    deriving (Show, Ord, Eq)

newtype TyVar = TyVar String
    deriving (Show, Ord, Eq)

type TyCons = (TyEx, TyEx)

data StTy = Pure | Mixed
    deriving (Show, Eq)

data TwEx =
            QInit Maybe TwTy | Var String Maybe TwTy | FunVar String Maybe TwTy
            | U1 TwEx Maybe TwTy | U2 TwEx Maybe TwTy
            | LetEx TwEx TwEx TwEx Maybe TwTy  |  App TwEx TwEx Maybe TwTy | Pair TwEx Maybe TwTy
            | ITE TwEx TwEx TwEx Maybe TwTy| TwT Maybe TwTy | TwF Maybe TwTy | Msr TwEx Maybe TwTy
            | MkEnt StTy TwEx Maybe TwTy | Split StTy TwEx Maybe TwTy | Cast StTy TwEx Maybe TwTy

data TwProg = Fun String String TwEx TwProg Maybe TwTy | Main TwEx Maybe TwTy

-- (foldl Set.union Set.empty (map (\x -> Set.union (freeVars $ fst x)  (freeVars $ snd x))  (genConstraints expr)))

-- genConstraints :: Set.Set TyVar -> TwEx -> [TyCons]
-- genConstraints currentVars (LetEx lhs rhs expr) = (PlainVar lty, PlainVar rty) : genConstraints (Set.union (Set.fromList [lty, rty]) currentVars) expr
--      where [lty, rty] = freshVars 2 currentVars
-- genConstraints currentVars QInit = []
-- genConstraints currentVars (U1 expr) = (PlainVar tyv, Exactly (Mixed Qubit)) : genConstraints (Set.union (Set.fromList [tyv]) currentVars) expr
--     where [tyv] = freshVars 1 currentVars

solveConstraints :: [TyCons] -> Maybe (Map.Map TyEx TyEx)
solveConstraints [] = Just Map.empty
solveConstraints (eq : rest) =
    case eq of
        (lhs, rhs) | lhs == rhs -> solveConstraints rest
        (PlainVar x, rhs) | not $ Set.member x (freeVars rhs) ->  Map.union <$> Just (Map.singleton (PlainVar x) rhs) <*> solveConstraints (map (tMap (subst (PlainVar x) rhs)) rest)
        (lhs, PlainVar y) | not $ Set.member y (freeVars lhs) ->  Map.union <$> Just (Map.singleton (PlainVar y) lhs) <*> solveConstraints (map (tMap (subst (PlainVar y) lhs)) rest)
        (FuncEx t1 t2, FuncEx s1 s2) -> solveConstraints ((t1, s1):(t2,s2):rest)
        (ProdEx t1 t2, ProdEx s1 s2) -> solveConstraints ((t1, s1):(t2,s2):rest)
        (lhs, rhs) -> Nothing


freeVars :: TyEx -> Set.Set TyVar
freeVars (PlainVar var) = Set.singleton var
freeVars (Exactly _) = Set.empty
freeVars (FuncEx left right) = Set.union (freeVars left) (freeVars right)
freeVars (ProdEx left right) = Set.union (freeVars left) (freeVars right)

subst :: TyEx -> TyEx -> TyEx -> TyEx
subst before after expr | expr == before = after
subst before after (FuncEx ty1 ty2) = FuncEx (subst before after ty1) (subst before after ty2)
subst before after (ProdEx ty1 ty2) = ProdEx (subst before after ty1) (subst before after ty2)
subst  _ _ expr = expr

tMap :: (a->b) -> (a,a) -> (b,b)
tMap f (x,y) = (f x, f y)

freshVars :: Int -> Set.Set TyVar -> [TyVar]
freshVars n tyVars = map TyVar (take n [s | s <- allStringsLetters, not (Set.member s strs)])
    where
        extractStr (TyVar x) = x
        strs = Set.union (Set.singleton "") (Set.map extractStr tyVars)
        allStringsLetters = [c : s | s <- "" : allStringsLetters, c <- ['a'..'z']]
