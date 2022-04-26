module TwistParsing (parseTwist) where

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

parsePurity :: GenParser Char st StTy
parsePurity = try (do parseAngles $ parseChar 'P'
                      return Pure)
          <|> try (do parseAngles $ parseChar 'M'
                      return Mixed)

parseQuantumTypeQubit :: GenParser Char st QTy
parseQuantumTypeQubit = try (do parseString "qubit"
                                return Qubit)
                    <|> parseParentheses parseQuantumTypePair

parseQuantumTypePair :: GenParser Char st QTy
parseQuantumTypePair = try (do t1 <- parseQuantumTypeQubit
                               parseChar '&'
                               Ent t1 <$> parseQuantumTypeQubit)
                   <|> parseQuantumTypeQubit

parseQuantumType :: GenParser Char st QTy
parseQuantumType = parseQuantumTypePair

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

parseType :: GenParser Char st TwTy
parseType = parseTypeFunction

parseTwist :: String -> Either ParseError TwTy
parseTwist = parse parseType ""
