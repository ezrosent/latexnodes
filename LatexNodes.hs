{-# LANGUAGE LambdaCase #-}
module LatexNodes where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), optional, many)
import Text.Printf (printf)

-- TODO: - parse to LText directly (see between combinator)
--       - decide on bullets, delimiters for
--       - math
--       - displaymath
--       - inline tex

type Bullet = (Int,String)

data LText = Normal String | Math String | DMath String
  deriving (Eq,Show)

data Doc = Doc String String [Bullet]
  deriving (Eq,Show)

parseHead :: String -> Parser String
parseHead hd = string hd *> many (noneOf "\n")

parseBull :: Char -> Parser Bullet
parseBull b = do
  nt   <- ntabs <* char b
  text <- many (noneOf "\t")
  return (nt,text)
  where ntabs    = ntabs' 0
        ntabs' n = try (char '\t' *> ntabs' (n+1)) <|> pure n

parseDoc :: Parser Doc
parseDoc = do
  title'  <- title  <* string "\n"
  author' <- author <* string "\n"
  buls    <- manyTill (parseBull '-') (eof *> pure [])
  return (Doc title' author' buls)
  where title  = parseHead "TITLE:"
        author = parseHead "AUTHOR:"

-- simple parser for math-escaped text
parseLText :: Parser [LText]
parseLText = manyTill lTtoken (eof *> pure [])
  where lTtoken = try  $  Normal <$> (many1  $ noneOf "#")
                      <|> Math   <$> (braces $ many1 (noneOf "#"))
        braces = between (char '#') (char '#')

-- standard test thing for ghci
ptest :: Show a => Parser a -> String -> IO ()
ptest a s = print $ parse a "" s

stringOfLText :: LText -> String
stringOfLText = \case
                   Math s -> printf "\\( %s \\)" s
                   Normal s -> s

emitDoc :: Doc -> String
emitDoc (Doc title author bullets) = title' ++ author' ++ bullets'
  where title'   = printf "\\title{%s}" title
        author'  = printf "\\author{%s}" author
        bullets' = concatMap emitLT finalB
        finalB = map lTextofBullet $ {- collapse  -} bullets
        emitLT :: (Int,[LText]) -> String
        emitLT (n,lts) = concatMap id (replicate n "\n\\begin{itemize}\\n") ++ lts' 
                         ++  concatMap id (replicate n "\n\\end{itemize}")
          where lts' = concatMap stringOfLText lts
        lTextofBullet :: Bullet -> (Int,[LText])
        lTextofBullet (n,tex) = (n,rightOf $ parse parseLText "" tex)
        rightOf = \case
                     Right a -> Normal "\\item " : a
                     Left err -> error  $ "some sort of parse error happened" ++ show err
