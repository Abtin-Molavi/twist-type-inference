module TwistParsing (TwistParsing.parse, unparse) where

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

parseParameter :: GenParser Char st TwEx
parseParameter = do id <- parseIdentifier
                    Var id <$> parseType

parseExpressionVariable :: GenParser Char st TwEx
parseExpressionVariable = try (do parseString "true"
                                  return $ TwT $ Just TwBool)
                      <|> try (do parseString "false"
                                  return $ TwT $ Just TwBool)
                      <|> try (do id <- parseIdentifier
                                  return $ Var id Nothing)
                      <|> parseParentheses parseExpression

parseExpressionPair :: GenParser Char st TwEx
parseExpressionPair = try (do parseChar '('
                              e1 <- parseExpression
                              parseChar ','
                              e2 <- parseExpression
                              parseChar ')'
                              return $ Pair e1 e2 Nothing)
                  <|> parseExpressionVariable

parseExpressionArguments :: GenParser Char st TwEx
parseExpressionArguments = try (do parseChar '('
                                   parseChar ')'
                                   return $ VarNull Nothing)
                       <|> parseExpressionPair

parseExpressionUnitary1Operator :: GenParser Char st String
parseExpressionUnitary1Operator = try (parseString "H")
                              <|> try (parseString "X")
                              <|> try (parseString "Y")
                              <|> try (parseString "Z")

parseExpressionUnitary1 :: GenParser Char st TwEx
parseExpressionUnitary1 = do u <- parseExpressionUnitary1Operator
                             e <- parseExpressionPair
                             return $ U1 u e Nothing

parseExpressionUnitary2Operator :: GenParser Char st String
parseExpressionUnitary2Operator = try (parseString "CNOT")
                              <|> try (parseString "CZ")
                              <|> try (parseString "TOF")
                              <|> try (parseString "FRED")

parseExpressionUnitary2 :: GenParser Char st TwEx
parseExpressionUnitary2 = do u <- parseExpressionUnitary2Operator
                             e <- parseExpressionPair
                             return $ U2 u e Nothing

parseExpressionMeasure :: GenParser Char st TwEx
parseExpressionMeasure = do parseString "measure"
                            e <- parseExpressionPair
                            return $ Msr e Nothing

parseExpressionQInit :: GenParser Char st TwEx
parseExpressionQInit = do parseString "qinit"
                          parseChar '('
                          parseChar ')'
                          return $ QInit $ Just $ QuantTy Pure Qubit

parseExpressionEntangle :: GenParser Char st TwEx
parseExpressionEntangle = do parseString "entangle"
                             p <- parsePurity
                             e <- parseExpressionPair
                             return $ MkEnt p e Nothing

parseExpressionSplit :: GenParser Char st TwEx
parseExpressionSplit = do parseString "split"
                          p <- parsePurity
                          e <- parseExpressionPair
                          return $ Split p e Nothing

parseExpressionCast :: GenParser Char st TwEx
parseExpressionCast =  do parseString "cast"
                          p <- parsePurity
                          e <- parseExpressionPair
                          return $ Cast p e Nothing

parseExpressionApplication :: GenParser Char st TwEx
parseExpressionApplication = try parseExpressionUnitary1
                         <|> try parseExpressionUnitary2
                         <|> try parseExpressionMeasure
                         <|> try parseExpressionQInit
                         <|> try parseExpressionEntangle
                         <|> try parseExpressionSplit
                         <|> try parseExpressionCast
                         <|> try (do e1 <- parseExpressionPair
                                     e2 <- parseExpressionArguments
                                     return $ App e1 e2 Nothing)
                         <|> parseExpressionPair

-- TODO: Phase shifts

parseExpressionIf :: GenParser Char st TwEx
parseExpressionIf = try (do parseString "if"
                            b <- parseExpression
                            parseString "then"
                            e1 <- parseExpression
                            parseString "else"
                            e2 <- parseExpression
                            return $ ITE b e1 e2 Nothing)
                <|> parseExpressionApplication

parseExpressionLetParameters :: GenParser Char st TwEx
parseExpressionLetParameters = try (do parseChar '('
                                       e1 <- parseParameter
                                       parseChar ','
                                       e2 <- parseParameter
                                       parseChar ')'
                                       Pair e1 e2 <$> parseType)
                           <|> try (do e <- parseParameter
                                       return e)
                           <|> parseParentheses parseExpressionLetParameters

parseExpressionLet :: GenParser Char st TwEx
parseExpressionLet = try (do parseString "let"
                             x <- parseExpressionLetParameters
                             parseChar '='
                             e1 <- parseExpression
                             parseString "in"
                             e2 <- parseExpression
                             return (case x of
                                          (Pair _ _ _) -> LetEx x e1 e2 Nothing
                                          x            -> LetEx (Pair x (Var "_" (Just TwBool)) Nothing)
                                                                (Pair e1 (TwT Nothing) Nothing)
                                                                e2 Nothing))
                 <|> parseExpressionIf

parseExpression :: GenParser Char st TwEx
parseExpression = parseExpressionLet

-- Programs
parseProgramFunctionParameters :: GenParser Char st TwEx
parseProgramFunctionParameters = try (do parseChar '('
                                         parseChar ')'
                                         return $ VarNull Nothing)
                             <|> try (parseParentheses parseParameter)

parseProgram :: GenParser Char st TwProg
parseProgram = try (do spaces
                       parseString "fun"
                       parseString "main"
                       parseChar '('
                       parseChar ')'
                       t <- parseType
                       parseChar '='
                       e <- parseExpression
                       eof
                       return $ Main e t)
           <|> try (do spaces
                       parseString "fun"
                       f <- parseIdentifier
                       x <- parseProgramFunctionParameters
                       t <- parseType
                       parseChar '='
                       e <- parseExpression
                       m <- parseProgram
                       return $ Fun f x e m t)

parse :: String -> Either ParseError TwProg
parse = Text.ParserCombinators.Parsec.parse parseProgram ""

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

unparseExpression :: TwEx -> String
unparseExpression (QInit _) = "qinit ()"
unparseExpression (Var x (Just t)) = x ++ " : " ++ unparseType t
unparseExpression (Var x Nothing) = x
unparseExpression (U1 u e _) = u ++ " (" ++ unparseExpression e ++ ")"
unparseExpression (U2 u (Pair e1 e2 t) _) = u ++ " " ++ unparseExpression (Pair e1 e2 t)
unparseExpression (U2 u e _) = u ++ " (" ++ unparseExpression e ++ ")"
unparseExpression (LetEx (Pair x (Var "_" (Just TwBool)) _) (Pair e1 (TwT Nothing) _) e2 _) = "let " ++ unparseExpression x ++ " = " ++ unparseExpression e1 ++ " in\n\t" ++ unparseExpression e2
unparseExpression (LetEx x e1 e2 _) = "let " ++ unparseExpression x ++ " = " ++ unparseExpression e1 ++ " in\n\t" ++ unparseExpression e2
unparseExpression (ITE b e1 e2 _) = "if " ++ unparseExpression b ++ " then " ++ unparseExpression e1 ++ " else " ++ unparseExpression e2
unparseExpression (App e1 e2 _) = unparseExpression e1 ++ " (" ++ unparseExpression e2 ++ ")"
unparseExpression (Pair e1 e2 _) = "(" ++ unparseExpression e1 ++ ", " ++ unparseExpression e2 ++ ")"
unparseExpression (TwT _) = "true"
unparseExpression (TwF _) = "false"
unparseExpression (Msr e _) = "measure (" ++ unparseExpression e ++ ")"
unparseExpression (MkEnt p (Pair e1 e2 t) _) = "entangle<" ++ unparsePurity p ++ ">" ++ unparseExpression (Pair e1 e2 t)
unparseExpression (MkEnt p e _) = "entangle<" ++ unparsePurity p ++ ">(" ++ unparseExpression e ++ ")"
unparseExpression (Split p (Pair e1 e2 t) _) = "split<" ++ unparsePurity p ++ ">" ++ unparseExpression (Pair e1 e2 t)
unparseExpression (Split p e _) = "split<" ++ unparsePurity p ++ ">(" ++ unparseExpression e ++ ")"
unparseExpression (Cast p (Pair e1 e2 t) _) = "cast<" ++ unparsePurity p ++ ">" ++ unparseExpression (Pair e1 e2 t)
unparseExpression (Cast p e _) = "cast<" ++ unparsePurity p ++ ">(" ++ unparseExpression e ++ ")"
unparseExpression _ = ""

unparseProgram :: TwProg -> String
unparseProgram (Fun f x e m (Just t)) =
    "fun " ++ f ++ " (" ++ unparseExpression x ++ ") : " ++ unparseType t ++ " =\n\t"
           ++ unparseExpression e ++ "\n\n" ++ unparseProgram m
unparseProgram (Fun f x e m Nothing) =
    "fun " ++ f ++ " (" ++ unparseExpression x ++ ") =\n\t"
           ++ unparseExpression e ++ "\n\n" ++ unparseProgram m
unparseProgram (Main e (Just t)) =
    "fun main () : " ++ unparseType t ++ " =\n\t" ++ unparseExpression e ++ "\n"
unparseProgram (Main e Nothing) =
    "fun main () =\n\t" ++ unparseExpression e ++ "\n"

unparse :: Either ParseError TwProg -> String
unparse (Left _) = "error"
unparse (Right m) = unparseProgram m
