{-# LANGUAGE MultiWayIf#-}
module Parser (parseLText, parseDoc)  where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), optional, many)
import AST

parseDelim :: Char -> Char -> Parser String
parseDelim = parseAcc 1
  where parseAcc :: Int -> Char -> Char -> Parser String
        parseAcc n dleft dright = do
        s1 <- many $ noneOf [dleft,dright]
        c <- anyChar
        s2 <- if | c == dright -> if n==1 then return s1
                        else do
                           inter <- parseAcc (n-1) dleft dright
                           return $ s1 ++ (dright:inter)
                 | c == dleft -> do
                           inter <- parseAcc (n+1) dleft dright
                           return $ s1 ++ (dleft:inter)
                 | True -> error "This shouldn't happen, there was a programmer error in parsing delimiters"
        return s2

square :: Parser String
square = parseDelim '[' ']'
{-curly :: Parser String-}
{-curly = parseDelim '{' '}'-}

bt :: String -> Parser a -> Parser a
bt b s = string b *> s <* string b
lexeme :: Parser a -> Parser a
lexeme p = (many $ oneOf  " \t\n\r") *> p <* (many $ oneOf " \t\n\r")
{-oneOfS :: [String] -> Parser String-}
{-oneOfS = choice . (map string)-}

--TODO: switch to manyTill with 'choice'
parseLText :: Parser [LText]
parseLText = manyTill (try  (Normal <$> (many1 $ noneOf "#~"))
          <|> try (Code <$>  (string "#code[" *> square) <*> (char '[' *> square))
          <|> try (do
            command <- string "#!" *> (many1 $ noneOf "\t \n[]") <* char '['
            body' <- square
            body <- case parse parseLText "" body' of
                      Left e -> error $ "parse error in nested code block:\n" ++ body' ++
                                "yielded the following error:\n" ++ (show e)
                      Right s -> return s
            return $ LBlock command body)
          <|> try (IVShell <$>  (string "#v##" *> (lexeme $ many1 (noneOf " #\t\n\r")))
                    <*>  many (lexeme (try (many1 $ oneOf " \n\t\r") -- whitespace-separated args
                                  <|> (many1 $ noneOf " \n\t\r#"))) <* string "###")
          <|> try (IShell <$>  (string "###" *> (lexeme $ many1 (noneOf " #\t\n\r")))
                    <*>  many (lexeme (try (many1 $ oneOf " \n\t\r")
                                  <|> (many1 $ noneOf " \n\t\r#"))) <* string "###")
          <|> try (Underline <$> (bt "##" $ many1 (noneOf "#")))
          <|> try (Bold      <$> (bt "#"  $ many1 (noneOf "#")))
          <|> IText <$> (bt "~" $ many1 (noneOf "#~"))) $
              (eof *> pure "") *> pure []

parseSection :: Parser [DocElts]
parseSection = manyTill section $ (lookAhead $ try $ many1 (char '\t') *> pure [])
  where section = try (Section <$> (string "Section:" *> (many1 (noneOf "\t\n\r")) <* char '\n'))
              <|> try (SubSection    <$>
                      (string "Sub:" *> (many1 (noneOf "\t\n\r")) <* char '\n'))
              <|> try (SubSubSection <$>
                      (string "Subsub:" *> (many1 (noneOf "\t\n\r")) <* char '\n'))
              <|> try (Paragraph     <$> (manyTill anyChar $ (try (string "\n\n"))))

parseHead :: String -> Parser String
parseHead hd = string hd *> many (noneOf "\n")

parseBull :: Char -> Parser Bullet
parseBull b = ( , ) <$> (ntabs 0 <* char b) <*> manyTill anyChar --(noneOf [])
              (lookAhead $ try (many1 (char '\t') <* char b ) <|>
              (lookAhead $ try (string "\n\n")) <|>  eof *> string "" )
  where ntabs n = try (char '\t' *> ntabs (n+1)) <|> pure n

parseDoc :: Parser Doc
parseDoc = Doc <$> (parseHead "TITLE:"  <* string "\n")
               <*> (parseHead "AUTHOR:" <* string "\n")
               <*> many1 parseRest
  where parseRest = ( , ) <$> parseSection <*>
                          (try (eof *> pure []) 
                                <|> try (string "\n\n" *> pure [])
                                <|>     (manyTill (parseBull '-') $ (try (eof *> pure [])
                                                                         <|> (string "\n\n") *> pure [])))
