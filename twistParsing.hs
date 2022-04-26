module TwistParsing (TwistParsing.parse) where

import Data.Char
import Text.ParserCombinators.Parsec
import TwistAST

parseString :: String -> GenParser Char st String
parseString s = string s <* spaces

parseChar :: Char -> GenParser Char st Char
parseChar c = char c <* spaces

parseAngles :: GenParser Char st a -> GenParser Char st a
parseAngles = between (parseChar '<') (parseChar '>')

parseParentheses :: GenParser Char st a -> GenParser Char st a
parseParentheses = between (parseChar '(') (parseChar ')')

-- Purities
parsePurity :: GenParser Char st StTy
parsePurity = try (do parseAngles $ parseChar 'P'
                      return Pure)
          <|> try (do parseAngles $ parseChar 'M'
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
                                       return $ Pair e (Var "_" $ Just TwBool) Nothing)
                           <|> parseParentheses parseExpressionLetParameters

parseExpressionLet :: GenParser Char st TwEx
parseExpressionLet = try (do parseString "let"
                             x <- parseExpressionLetParameters
                             parseChar '='
                             e1 <- parseExpression
                             parseString "in"
                             e2 <- parseExpression
                             return $ LetEx x e1 e2 Nothing)
                 <|> parseExpressionIf

parseExpression :: GenParser Char st TwEx
parseExpression = parseExpressionLet

parse :: String -> Either ParseError TwEx
parse = Text.ParserCombinators.Parsec.parse parseExpression ""
