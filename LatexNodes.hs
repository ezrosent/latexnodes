{-# LANGUAGE LambdaCase,TupleSections,MultiWayIf #-}
module LatexNodes where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), optional, many)
import Text.Printf (printf)

type Bullet = (Int,String)

data LText = Normal String | Math String | DMath String
  deriving (Eq,Show)

data Doc = Doc String String [Bullet]
  deriving (Eq,Show)

parseHead :: String -> Parser String
parseHead hd = string hd *> many (noneOf "\n")

parseBull :: Char -> Parser Bullet
parseBull b = ( , ) <$> (ntabs 0 <* char b) <*> many (noneOf "\t")
  where ntabs n = try (char '\t' *> ntabs (n+1)) <|> pure n

parseDoc :: Parser Doc
parseDoc = Doc <$> (parseHead "TITLE:" <* string "\n")
               <*> (parseHead "AUTHOR:" <* string "\n")
               <*> (manyTill (parseBull '-') (eof *> pure []))

-- simple parser for math-escaped text
parseLText :: Parser [LText]
parseLText = manyTill lTtoken (eof *> pure [])
  where lTtoken = try $ Normal <$> (many1   $ noneOf "#")
              <|> try  (DMath  <$> (dbraces $ many1 (noneOf "#")))
              <|>       Math   <$> (braces  $ many1 (noneOf "#"))
        dbraces s = string "##" *> s <* string "##"
        braces    = between (char '#') (char '#')

--- end parsing section
stringOfLText :: LText -> String
stringOfLText = \case Math s   -> printf "\\( %s \\)" s
                      DMath s  -> printf "\\[ %s \\]" s
                      Normal s -> s

-- decalre header-specific variables based on title and author
-- TODO: don't hardcode author/title variables
varDecs :: Doc -> String
varDecs (Doc title author _) = printf "\\newcommand{\\mytitle}{%s}\
  \ \n\\newcommand{\\myauthor}{%s}" title author
-- emit laTex from a document
-- TODO: have it return either rather than ad-hoc error throwing
emitDoc :: Doc -> String
emitDoc (Doc title author bullets) = printf "%s\n%s\n\\begin{document}\
  \ \n \\maketitle\n%s\n\\end{document}" title' author'  bullets'
  where title'   = (printf "\\title{%s}" title)   :: String
        author'  = (printf "\\author{%s}" author) :: String
        bullets' = emitBullet $ map lTextofBullet bullets
        emitBullet = emitBT True . conv
        lTextofBullet :: Bullet -> (Int,[LText])
        lTextofBullet (n,tex) = (n,rightOf $ parse parseLText "" tex)
        rightOf = \case Right a  -> Normal "\\item " : a
                        Left err -> error  $ "some sort of parse error happened" ++ show err

-- intermediate data structure to hold bullets
-- it has [list of bullets], optional children, optional continuation
-- of bullets at same level after children, so It's a binary tree
data  BT a = B [a] (Maybe (BT a)) (Maybe (BT a))
  deriving (Eq,Show)

-- convert an assoc-list into a BT,
-- the assoc-list has the semantics of 'Indentation Levels'
conv :: Eq a => [(Int,a)] -> BT a
conv = \case
  []           -> B [] Nothing Nothing
  xs@((n,_):_) -> B (map snd curr)
                    (if (child == []) || (fst $ head child) < n then Nothing
                             else Just $ conv child)
                    (if (next == []) then Nothing else Just $ conv next)
    where (curr,child,next) = split3 ((== n).fst) xs
          split3 f ls = (spl takeWhile,takeWhile (not.f) mid,end)
            where spl g = g f ls
                  mid = spl dropWhile
                  end = dropWhile (not.f) mid

-- convert BT [LText] to well-itemized Latex;
-- Boolean flag indicates whether or not to begin with a \begin{itemize}
emitBT :: Bool ->  BT [LText] -> String
emitBT b (B ls next cont)  = printf "\
\%s%s%s%s"   (if b then "\\begin{itemize}\n" else "") bullets children continue
  where bullets = concatMap id $ map (concatMap $ (++ "\n") .  stringOfLText) ls
        children = case next of {Nothing -> ""; Just s -> emitBT True s}
        continue = case cont of {Nothing -> "\\end{itemize}\n"; Just s -> emitBT False s}
