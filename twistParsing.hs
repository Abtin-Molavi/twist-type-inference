module TwistParsing (TwistParsing.parse, unparse, unparseVerbose) where

import Data.Char
import Text.ParserCombinators.Parsec
import TwistAST

parseString :: String -> GenParser Char st String
parseString s = do string s
                   notFollowedBy $ satisfy $
                           \c -> (c `notElem` "()<>&*=-:,") && not (isSpace c)
                   spaces
                   return s

parseChar :: Char -> GenParser Char st Char
parseChar c = char c <* spaces

parseAngles :: GenParser Char st a -> GenParser Char st a
parseAngles = between (parseChar '<') (parseChar '>')

parseParentheses :: GenParser Char st a -> GenParser Char st a
parseParentheses = between (parseChar '(') (parseChar ')')

-- Purities
parsePurity :: GenParser Char st StTy
parsePurity = try (do parseAngles $ parseString "P"
                      return Pure)
          <|> try (do parseAngles $ parseString "M"
                      return Mixed)

-- Quantum Types
parseQuantumTypeQubit :: GenParser Char st QTy
parseQuantumTypeQubit = try (do parseString "qubit"
                                return Qubit)
                    <|> parseParentheses parseQuantumType

parseQuantumTypePair :: GenParser Char st QTy
parseQuantumTypePair = try (do t1 <- parseQuantumTypeQubit
                               parseChar '&'
                               Ent t1 <$> parseQuantumTypeQubit)
                   <|> parseQuantumTypeQubit

parseQuantumType :: GenParser Char st QTy
parseQuantumType = parseQuantumTypePair

-- Types
parseTypeBool :: GenParser Char st TwTy
parseTypeBool = try (do parseString "bool"
                        return TwBool)
            <|> parseParentheses parseTypeFunction

parseTypeQubit :: GenParser Char st TwTy
parseTypeQubit = try (do q <- parseQuantumType
                         p <- parsePurity
                         return $ QuantTy p q)
             <|> parseTypeBool

parseTypeProduct :: GenParser Char st TwTy
parseTypeProduct = try (do t1 <- parseTypeQubit
                           parseChar '*'
                           Prod t1 <$> parseTypeQubit)
               <|> parseTypeQubit

parseTypeFunction :: GenParser Char st TwTy
parseTypeFunction = try (do t1 <- parseTypeQubit
                            parseString "->"
                            Func t1 <$> parseTypeQubit)
                <|> parseTypeProduct

parseType :: GenParser Char st (Maybe TwTy)
parseType = do parseChar ':'
               Just <$> parseTypeFunction
        <|> return Nothing

-- Expressions
parseIdentifier :: GenParser Char st String
parseIdentifier = do notFollowedBy $ parseString "fun"
                     notFollowedBy $ parseString "let"
                     notFollowedBy $ parseString "in"
                     notFollowedBy $ parseString "if"
                     notFollowedBy $ parseString "then"
                     notFollowedBy $ parseString "else"
                     notFollowedBy $ parseString "true"
                     notFollowedBy $ parseString "false"
                     notFollowedBy $ parseString "qinit"
                     notFollowedBy $ parseString "H"
                     notFollowedBy $ parseString "X"
                     notFollowedBy $ parseString "Y"
                     notFollowedBy $ parseString "Z"
                     notFollowedBy $ parseString "CNOT"
                     notFollowedBy $ parseString "CZ"
                     notFollowedBy $ parseString "TOF"
                     notFollowedBy $ parseString "FRED"
                     notFollowedBy $ parseString "measure"
                     notFollowedBy $ parseString "entangle"
                     notFollowedBy $ parseString "split"
                     notFollowedBy $ parseString "cast"
                     first <- satisfy $ \x -> isAlpha x || x == '_'
                     rest <- many $ satisfy $
                             \x -> isAlpha x || isDigit x || x == '_'
                     spaces
                     return (first:rest)

parseParameter :: String -> GenParser Char st TwEx
parseParameter label = do id <- parseIdentifier
                          t <- parseType
                          return $ Var id t label

parseExpressionVariable :: String -> GenParser Char st TwEx
parseExpressionVariable label = try (do parseString "true"
                                        return $ TwT (Just TwBool) label)
                            <|> try (do parseString "false"
                                        return $ TwT (Just TwBool) label)
                            <|> try (do id <- parseIdentifier
                                        return $ Var id Nothing label)
                            <|> parseParentheses (parseExpression label)

parseExpressionPair :: String -> GenParser Char st TwEx
parseExpressionPair label = try (do parseChar '('
                                    e1 <- parseExpression ("0" ++ label)
                                    parseChar ','
                                    e2 <- parseExpression ("1" ++ label)
                                    parseChar ')'
                                    return $ Pair e1 e2 Nothing label)
                        <|> parseExpressionVariable label

parseExpressionArguments :: String -> GenParser Char st TwEx
parseExpressionArguments label = try (do parseChar '('
                                         parseChar ')'
                                         return $ VarNull Nothing label)
                             <|> parseExpressionPair label

parseExpressionUnitary1Operator :: GenParser Char st String
parseExpressionUnitary1Operator = try (parseString "H")
                              <|> try (parseString "X")
                              <|> try (parseString "Y")
                              <|> try (parseString "Z")

parseExpressionUnitary1 :: String -> GenParser Char st TwEx
parseExpressionUnitary1 label = do u <- parseExpressionUnitary1Operator
                                   e <- parseExpressionPair ("0" ++ label)
                                   return $ U1 u e Nothing label

parseExpressionUnitary2Operator :: GenParser Char st String
parseExpressionUnitary2Operator = try (parseString "CNOT")
                              <|> try (parseString "CZ")
                              <|> try (parseString "TOF")
                              <|> try (parseString "FRED")

parseExpressionUnitary2 :: String -> GenParser Char st TwEx
parseExpressionUnitary2 label = do u <- parseExpressionUnitary2Operator
                                   e <- parseExpressionPair ("0" ++ label)
                                   return $ U2 u e Nothing label

parseExpressionMeasure :: String -> GenParser Char st TwEx
parseExpressionMeasure label = do parseString "measure"
                                  e <- parseExpressionPair ("0" ++ label)
                                  return $ Msr e Nothing label

parseExpressionQInit :: String -> GenParser Char st TwEx
parseExpressionQInit label = do parseString "qinit"
                                parseChar '('
                                parseChar ')'
                                return $ QInit Nothing label

parseExpressionEntangle :: String -> GenParser Char st TwEx
parseExpressionEntangle label = do parseString "entangle"
                                   p <- parsePurity
                                   e <- parseExpressionPair ("0" ++ label)
                                   return $ MkEnt p e Nothing label

parseExpressionSplit :: String -> GenParser Char st TwEx
parseExpressionSplit label = do parseString "split"
                                p <- parsePurity
                                e <- parseExpressionPair ("0" ++ label)
                                return $ Split p e Nothing label

parseExpressionCast :: String -> GenParser Char st TwEx
parseExpressionCast label = do parseString "cast"
                               p <- parsePurity
                               e <- parseExpressionPair ("0" ++ label)
                               return $ Cast p e Nothing label

parseExpressionApplication :: String -> GenParser Char st TwEx
parseExpressionApplication label =
        try (parseExpressionUnitary1 label)
    <|> try (parseExpressionUnitary2 label)
    <|> try (parseExpressionMeasure label)
    <|> try (parseExpressionQInit label)
    <|> try (parseExpressionEntangle label)
    <|> try (parseExpressionSplit label)
    <|> try (parseExpressionCast label)
    <|> try (do e1 <- parseExpressionPair ("0" ++ label)
                e2 <- parseExpressionArguments ("1" ++ label)
                return $ App e1 e2 Nothing label)
    <|> parseExpressionPair label

-- TODO: Phase shifts

parseExpressionIf :: String -> GenParser Char st TwEx
parseExpressionIf label = try (do parseString "if"
                                  b <- parseExpression ("0" ++ label)
                                  parseString "then"
                                  e1 <- parseExpression ("1" ++ label)
                                  parseString "else"
                                  e2 <- parseExpression ("2" ++ label)
                                  return $ ITE b e1 e2 Nothing label)
                      <|> parseExpressionApplication label

parseExpressionLetParameters :: String -> GenParser Char st TwEx
parseExpressionLetParameters label =
        try (do parseChar '('
                e1 <- parseParameter ("0" ++ label)
                parseChar ','
                e2 <- parseParameter ("1" ++ label)
                parseChar ')'
                t <- parseType
                return $ Pair e1 e2 t label)
    <|> try (do e <- parseParameter ("0" ++ label)
                return e)
    <|> parseParentheses (parseExpressionLetParameters label)

parseExpressionLet :: String -> GenParser Char st TwEx
parseExpressionLet label =
        try (do parseString "let"
                x <- parseExpressionLetParameters ("0" ++ label)
                parseChar '='
                e1 <- (case x of
                            Pair _ _ _ _ -> parseExpression ("1" ++ label)
                            _            -> parseExpression ("01" ++ label))
                parseString "in"
                e2 <- parseExpression ("2" ++ label)
                return (case x of
                             Pair _ _ _ _ -> LetEx x e1 e2 Nothing label
                             Var  x' t _  ->
                                     LetEx (Pair (Var x' t ("00" ++ label))
                                                 (Var "_"
                                                      (Just TwBool)
                                                      ("10" ++ label))
                                                 Nothing ("0" ++ label))
                                           (Pair e1
                                                 (TwT Nothing ("11" ++ label))
                                                 Nothing ("1" ++ label))
                                           e2
                                           Nothing
                                           label))
    <|> parseExpressionIf label

parseExpression :: String -> GenParser Char st TwEx
parseExpression label = parseExpressionLet label

-- Programs
parseProgramFunctionParameters :: String -> GenParser Char st TwEx
parseProgramFunctionParameters label =
        try (do parseChar '('
                parseChar ')'
                return $ VarNull Nothing label)
    <|> try (parseParentheses (parseParameter label))

parseProgram :: Int -> GenParser Char st TwProg
parseProgram label = try (do spaces
                             parseString "fun"
                             parseString "main"
                             parseChar '('
                             parseChar ')'
                             t <- parseType
                             parseChar '='
                             e <- parseExpression ("1.0")
                             eof
                             return $ Main e t "0")
                 <|> try (do spaces
                             parseString "fun"
                             f <- parseIdentifier
                             x <- parseProgramFunctionParameters
                                     ("0." ++ show label)
                             t <- parseType
                             parseChar '='
                             e <- parseExpression ("1." ++ show label)
                             m <- parseProgram (label + 1)
                             return $ Fun f x e m t $ show label)

parse :: String -> Either ParseError TwProg
parse = Text.ParserCombinators.Parsec.parse (parseProgram 1) ""

unparseQuantumType :: QTy -> String
unparseQuantumType Qubit = "qubit"
unparseQuantumType (Ent q1 q2) =
        "(" ++ unparseQuantumType q1 ++ " & " ++ unparseQuantumType q2 ++ ")"

unparsePurity :: StTy -> String
unparsePurity Pure = "P"
unparsePurity Mixed = "M"

unparseType :: TwTy -> String
unparseType TwBool = "bool"
unparseType (QuantTy p q) =
        unparseQuantumType q ++ "<" ++ unparsePurity p ++ ">"
unparseType (Prod t1 t2) =
        "(" ++ unparseType t1 ++ " * " ++ unparseType t2 ++ ")"
unparseType (Func t1 t2) = unparseType t1 ++ " -> " ++ unparseType t2

unparseExpression :: (String -> (Maybe TwTy) -> String -> String)
        -> TwEx -> String
unparseExpression annotator (Var x t label) = annotator x t label
unparseExpression annotator (VarNull _ _) = ""
unparseExpression annotator (App e1 e2 t label) =
        annotator (unparseExpression annotator e1
                ++ " (" ++ unparseExpression annotator e2 ++ ")") t label
unparseExpression annotator (Pair e1 e2 t label) =
        annotator ("("
                ++ unparseExpression annotator e1
                ++ ", "
                ++ unparseExpression annotator e2
                ++ ")") t label
unparseExpression annotator (QRef _ _ _) = ""
unparseExpression annotator (QPair _ _ _ _) = ""
-- unparseExpression annotator (LetEx (Pair x (Var "_" (Just TwBool) _) _ _)
--         (Pair e1 (TwT Nothing _) _ _) e2 t label) =
--         annotator ("let "
--                 ++ unparseExpression annotator x
--                 ++ " = "
--                 ++ unparseExpression annotator e1
--                 ++ " in\n    "
--                 ++ unparseExpression annotator e2) t label
unparseExpression annotator (LetEx x e1 e2 t label) =
        annotator ("let "
                ++ unparseExpression annotator x
                ++ " = "
                ++ unparseExpression annotator e1
                ++ " in\n    "
                ++ unparseExpression annotator e2) t label
unparseExpression annotator (ITE b e1 e2 t label) =
        annotator ("if "
                ++ unparseExpression annotator b
                ++ " then "
                ++ unparseExpression annotator e1
                ++ " else "
                ++ unparseExpression annotator e2) t label
unparseExpression annotator (TwT t label) = annotator "true" t label
unparseExpression annotator (TwF t label) = annotator "false" t label
unparseExpression annotator (QInit t label) = annotator "qinit ()" t label
unparseExpression annotator (U1 u e t label) =
        annotator (u ++ " (" ++ unparseExpression annotator e ++ ")") t label
unparseExpression annotator (U2 u e t label) =
        case e of
             Pair _ _ _ _ -> annotator
                     (u ++ " " ++ unparseExpression annotator e) t label
             _            -> annotator
                     (u ++ " (" ++ unparseExpression annotator e ++ ")")
                     t label
unparseExpression annotator (Msr e t label) =
        annotator ("measure (" ++ unparseExpression annotator e ++ ")") t label
unparseExpression annotator (MkEnt p e t label) =
    case e of
         Pair _ _ _ _ -> annotator ("entangle<"
                 ++ unparsePurity p
                 ++ ">"
                 ++ unparseExpression annotator e) t label
         _            -> annotator ("entangle<"
                 ++ unparsePurity p
                 ++ ">(" ++ unparseExpression annotator e ++ ")") t label
unparseExpression annotator (Split p e t label) =
    case e of
         Pair _ _ _ _ -> annotator ("split<"
                 ++ unparsePurity p
                 ++ ">"
                 ++ unparseExpression annotator e) t label
         _            -> annotator ("split<"
                 ++ unparsePurity p
                 ++ ">(" ++ unparseExpression annotator e ++ ")") t label
unparseExpression annotator (Cast p e t label) =
    case e of
         Pair _ _ _ _ -> annotator ("cast<"
                 ++ unparsePurity p
                 ++ ">"
                 ++ unparseExpression annotator e) t label
         _            -> annotator ("cast<"
                 ++ unparsePurity p
                 ++ ">(" ++ unparseExpression annotator e ++ ")") t label

unparseProgram :: (String -> (Maybe TwTy) -> String -> String)
        -> TwProg -> String
unparseProgram annotator (Fun f x e m s t) =
        annotator ("fun "
                ++ f
                ++ " (" ++ unparseExpression annotator x ++ ")") s t
                ++ " =\n    "
                ++ unparseExpression annotator e
                ++ "\n\n"
                ++ unparseProgram annotator m
unparseProgram annotator (Main e s t) =
        annotator "fun main ()" s t
                ++ " =\n    "
                ++ unparseExpression annotator e
                ++ "\n"

annotator :: String -> (Maybe TwTy) -> String -> String
annotator s t _ = case t of
                       Just t' -> s ++ " : " ++ unparseType t'
                       Nothing -> s

unparse :: TwProg -> String
unparse m = unparseProgram annotator m

annotatorVerbose :: String -> (Maybe TwTy) -> String -> String
annotatorVerbose s t label =
        case t of
             Just t' -> "(" ++ s ++ ")@" ++ label ++ " : " ++ unparseType t'
             Nothing -> "(" ++ s ++ ")@" ++ label ++ " : (?)"

unparseVerbose :: TwProg -> String
unparseVerbose m= unparseProgram annotatorVerbose m
