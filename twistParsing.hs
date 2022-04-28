module TwistParsing (TwistParsing.parse, unparse, unparseVerbose) where

import Data.Char
import Text.ParserCombinators.Parsec
import TwistAST

parseString :: String -> GenParser Char st String
parseString s = do string s
                   notFollowedBy $ satisfy $ \c -> (c `notElem` "()<>&*=-:,") && not (isSpace c)
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
                     rest <- many $ satisfy $ \x -> isAlpha x || isDigit x || x == '_'
                     spaces
                     return (first:rest)

parseParameter :: String -> GenParser Char st TwEx
parseParameter label = do id <- parseIdentifier
                          t <- parseType
                          return $ Var id (Annotation t label)

parseExpressionVariable :: String -> GenParser Char st TwEx
parseExpressionVariable label = try (do parseString "true"
                                        return $ TwT $ (Annotation (Just TwBool) label))
                            <|> try (do parseString "false"
                                        return $ TwT $ (Annotation (Just TwBool) label))
                            <|> try (do id <- parseIdentifier
                                        return $ Var id (Annotation Nothing label))
                            <|> parseParentheses (parseExpression label)

parseExpressionPair :: String -> GenParser Char st TwEx
parseExpressionPair label = try (do parseChar '('
                                    e1 <- parseExpression ("0" ++ label)
                                    parseChar ','
                                    e2 <- parseExpression ("1" ++ label)
                                    parseChar ')'
                                    return $ Pair e1 e2 (Annotation Nothing label))
                        <|> parseExpressionVariable label

parseExpressionArguments :: String -> GenParser Char st TwEx
parseExpressionArguments label = try (do parseChar '('
                                         parseChar ')'
                                         return $ VarNull (Annotation Nothing label))
                             <|> parseExpressionPair label

parseExpressionUnitary1Operator :: GenParser Char st String
parseExpressionUnitary1Operator = try (parseString "H")
                              <|> try (parseString "X")
                              <|> try (parseString "Y")
                              <|> try (parseString "Z")

parseExpressionUnitary1 :: String -> GenParser Char st TwEx
parseExpressionUnitary1 label = do u <- parseExpressionUnitary1Operator
                                   e <- parseExpressionPair ("0" ++ label)
                                   return $ U1 u e (Annotation Nothing label)

parseExpressionUnitary2Operator :: GenParser Char st String
parseExpressionUnitary2Operator = try (parseString "CNOT")
                              <|> try (parseString "CZ")
                              <|> try (parseString "TOF")
                              <|> try (parseString "FRED")

parseExpressionUnitary2 :: String -> GenParser Char st TwEx
parseExpressionUnitary2 label = do u <- parseExpressionUnitary2Operator
                                   e <- parseExpressionPair ("0" ++ label)
                                   return $ U2 u e (Annotation Nothing label)

parseExpressionMeasure :: String -> GenParser Char st TwEx
parseExpressionMeasure label = do parseString "measure"
                                  e <- parseExpressionPair ("0" ++ label)
                                  return $ Msr e (Annotation Nothing label)

parseExpressionQInit :: String -> GenParser Char st TwEx
parseExpressionQInit label = do parseString "qinit"
                                parseChar '('
                                parseChar ')'
                                return $ QInit (Annotation Nothing label)

parseExpressionEntangle :: String -> GenParser Char st TwEx
parseExpressionEntangle label = do parseString "entangle"
                                   p <- parsePurity
                                   e <- parseExpressionPair ("0" ++ label)
                                   return $ MkEnt p e (Annotation Nothing label)

parseExpressionSplit :: String -> GenParser Char st TwEx
parseExpressionSplit label = do parseString "split"
                                p <- parsePurity
                                e <- parseExpressionPair ("0" ++ label)
                                return $ Split p e (Annotation Nothing label)

parseExpressionCast :: String -> GenParser Char st TwEx
parseExpressionCast label = do parseString "cast"
                               p <- parsePurity
                               e <- parseExpressionPair ("0" ++ label)
                               return $ Cast p e (Annotation Nothing label)

parseExpressionApplication :: String -> GenParser Char st TwEx
parseExpressionApplication label = try (parseExpressionUnitary1 label)
                               <|> try (parseExpressionUnitary2 label)
                               <|> try (parseExpressionMeasure label)
                               <|> try (parseExpressionQInit label)
                               <|> try (parseExpressionEntangle label)
                               <|> try (parseExpressionSplit label)
                               <|> try (parseExpressionCast label)
                               <|> try (do e1 <- parseExpressionPair ("0" ++ label)
                                           e2 <- parseExpressionArguments ("1" ++ label)
                                           return $ App e1 e2 (Annotation Nothing label))
                               <|> parseExpressionPair label

-- TODO: Phase shifts

parseExpressionIf :: String -> GenParser Char st TwEx
parseExpressionIf label = try (do parseString "if"
                                  b <- parseExpression ("0" ++ label)
                                  parseString "then"
                                  e1 <- parseExpression ("1" ++ label)
                                  parseString "else"
                                  e2 <- parseExpression ("2" ++ label)
                                  return $ ITE b e1 e2 (Annotation Nothing label))
                      <|> parseExpressionApplication label

parseExpressionLetParameters :: String -> GenParser Char st TwEx
parseExpressionLetParameters label = try (do parseChar '('
                                             e1 <- parseParameter ("0" ++ label)
                                             parseChar ','
                                             e2 <- parseParameter ("0" ++ label)
                                             parseChar ')'
                                             t <- parseType
                                             return $ Pair e1 e2 (Annotation t label))
                                 <|> try (do e <- parseParameter ("1" ++ label)
                                             return e)
                                 <|> parseParentheses (parseExpressionLetParameters label)

parseExpressionLet :: String -> GenParser Char st TwEx
parseExpressionLet label =
        try (do parseString "let"
                x <- parseExpressionLetParameters ("0" ++ label)
                parseChar '='
                e1 <- (case x of
                            Pair _ _ _ -> parseExpression ("1" ++ label)
                            _          -> parseExpression ("10" ++ label))
                parseString "in"
                e2 <- parseExpression ("2" ++ label)
                return (case x of
                             Pair _ _ _              -> LetEx x e1 e2 (Annotation Nothing label)
                             Var x' (Annotation t _) -> LetEx (Pair (Var x' (Annotation t ("00" ++ label)))
                                                                    (Var "_" (Annotation (Just TwBool) ("01" ++ label)))
                                                                    (Annotation Nothing ("0" ++ label)))
                                                              (Pair e1
                                                                    (TwT (Annotation Nothing ("10" ++ label)))
                                                                    (Annotation Nothing ("1" ++ label)))
                                                              e2 (Annotation Nothing label)))
    <|> parseExpressionIf label

parseExpression :: String -> GenParser Char st TwEx
parseExpression label = parseExpressionLet label

-- Programs
parseProgramFunctionParameters :: String -> GenParser Char st TwEx
parseProgramFunctionParameters label = try (do parseChar '('
                                               parseChar ')'
                                               return $ VarNull (Annotation Nothing (label)))
                                   <|> try (parseParentheses (parseParameter label))

parseProgram :: Int -> GenParser Char st TwProg
parseProgram label = try (do spaces
                             parseString "fun"
                             parseString "main"
                             parseChar '('
                             parseChar ')'
                             t <- parseType
                             parseChar '='
                             e <- parseExpression ("0.0")
                             eof
                             return $ Main e (Annotation t "0"))
                 <|> try (do spaces
                             parseString "fun"
                             f <- parseIdentifier
                             x <- parseProgramFunctionParameters ("0." ++ show label)
                             t <- parseType
                             parseChar '='
                             e <- parseExpression ("1." ++ show label)
                             m <- parseProgram (label + 1)
                             return $ Fun f x e m (Annotation t $ show label))

parse :: String -> Either ParseError TwProg
parse = Text.ParserCombinators.Parsec.parse (parseProgram 1) ""

unparseQuantumType :: QTy -> String
unparseQuantumType Qubit = "qubit"
unparseQuantumType (Ent q1 q2) = "(" ++ unparseQuantumType q1 ++ " & " ++ unparseQuantumType q2 ++ ")"

unparsePurity :: StTy -> String
unparsePurity Pure = "P"
unparsePurity Mixed = "M"

unparseType :: TwTy -> String
unparseType TwBool = "bool"
unparseType (QuantTy p q) = unparseQuantumType q ++ "<" ++ unparsePurity p ++ ">"
unparseType (Prod t1 t2) = "(" ++ unparseType t1 ++ " * " ++ unparseType t2 ++ ")"
unparseType (Func t1 t2) = unparseType t1 ++ " -> " ++ unparseType t2

unparseExpression :: (String -> Annotation -> String) -> TwEx -> String
unparseExpression sas (QInit a) = sas "qinit ()" a
unparseExpression sas (Var x a) = sas x a
unparseExpression sas (U1 u e a) = sas (u ++ " (" ++ unparseExpression sas e ++ ")") a
unparseExpression sas (U2 u e a) =
    case e of
         Pair _ _ _ -> sas (u ++ " " ++ unparseExpression sas e) a
         _          -> sas (u ++ " (" ++ unparseExpression sas e ++ ")") a
unparseExpression sas (LetEx (Pair x (Var "_" (Annotation (Just TwBool) _)) _) (Pair e1 (TwT (Annotation Nothing _)) _) e2 a) =
    sas ("let " ++ unparseExpression sas x ++ " = " ++ unparseExpression sas e1 ++ " in\n\t" ++ unparseExpression sas e2) a
unparseExpression sas (LetEx x e1 e2 a) =
    sas ("let " ++ unparseExpression sas x ++ " = " ++ unparseExpression sas e1 ++ " in\n\t" ++ unparseExpression sas e2) a
unparseExpression sas (ITE b e1 e2 a) =
    sas ("if " ++ unparseExpression sas b ++ " then " ++ unparseExpression sas e1 ++ " else " ++ unparseExpression sas e2) a
unparseExpression sas (App e1 e2 a) = sas (unparseExpression sas e1 ++ " (" ++ unparseExpression sas e2 ++ ")") a
unparseExpression sas (Pair e1 e2 a) = sas ("(" ++ unparseExpression sas e1 ++ ", " ++ unparseExpression sas e2 ++ ")") a
unparseExpression sas (TwT a) = sas "true" a
unparseExpression sas (TwF a) = sas "false" a
unparseExpression sas (Msr e a) = sas ("measure (" ++ unparseExpression sas e ++ ")") a
unparseExpression sas (MkEnt p e a) =
    case e of
         Pair _ _ _ -> sas ("entangle<" ++ unparsePurity p ++ ">" ++ unparseExpression sas e) a
         _          -> sas ("entangle<" ++ unparsePurity p ++ ">(" ++ unparseExpression sas e ++ ")") a
unparseExpression sas (Split p e a) =
    case e of
         Pair _ _ _ -> sas ("split<" ++ unparsePurity p ++ ">" ++ unparseExpression sas e) a
         _          -> sas ("split<" ++ unparsePurity p ++ ">(" ++ unparseExpression sas e ++ ")") a
unparseExpression sas (Cast p e a) =
    case e of
         Pair _ _ _ -> sas ("cast<" ++ unparsePurity p ++ ">" ++ unparseExpression sas e) a
         _          -> sas ("cast<" ++ unparsePurity p ++ ">(" ++ unparseExpression sas e ++ ")") a
unparseExpression _ _ = ""

unparseProgram :: (String -> Annotation -> String) -> TwProg -> String
unparseProgram sas (Fun f x e m a) =
    sas ("fun " ++ f ++ " (" ++ unparseExpression sas x ++ ")") a ++ " =\n\t"
           ++ unparseExpression sas e ++ "\n\n" ++ unparseProgram sas m
unparseProgram sas (Main e a) =
    sas "fun main ()" a ++ " =\n\t" ++ unparseExpression sas e ++ "\n"

sas :: String -> Annotation -> String
sas s a = case a of
               Annotation (Just t) _  -> s ++ " : " ++ unparseType t
               Annotation Nothing _   -> s

unparse :: Either ParseError TwProg -> String
unparse (Left _) = "error"
unparse (Right m) = unparseProgram sas m

sasVerbose :: String -> Annotation -> String
sasVerbose s a = case a of
                      Annotation (Just t) label  -> "(" ++ s ++ ")@" ++ label ++ " : " ++ unparseType t
                      Annotation Nothing label   -> "(" ++ s ++ ")@" ++ label ++ " : (?)"

unparseVerbose :: Either ParseError TwProg -> String
unparseVerbose (Left _) = "error"
unparseVerbose (Right m) = unparseProgram sasVerbose m
