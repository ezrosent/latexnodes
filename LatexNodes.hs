{-# LANGUAGE LambdaCase,TupleSections,DeriveFunctor #-}
module LatexNodes where
import Text.ParserCombinators.Parsec
import System.Process
import Control.Applicative hiding ((<|>), optional, many)
import Text.Printf (printf)

type Bullet = (Int,String) -- (level -> content)

-- definitions of LaTeX tokens
data LText = Normal String | Math String | DMath String | IText String
  | IShell String [String]
  deriving (Eq, Show)

--- end parsing section
stringOfLText :: LText -> IO String
stringOfLText = \case Math s   -> return $ "\\(" ++ s ++ "\\)"
                      DMath s  -> return $ "\\[" ++ s ++ "\\]"
                      IText s  -> return $ "\\textit{" ++ s ++ "}"
                      Normal s -> return s
                      IShell s ls -> readProcess s ls "" >>=
                                     (\ r -> return $ "\\begin{verbatim}" ++ r
                                     ++ "\\end{verbatim}")

-- flexible type definition for parsing, makes it more extensible later
-- on if not all LTexts are strings

bt :: String -> Parser a -> Parser a
bt b s = string b *> s <* string b
lexeme :: Parser a -> Parser a
lexeme p = (many $ oneOf  " \t\n\r") *> p <* (many $ oneOf " \t\n\r")

parseLText :: Parser [LText]
parseLText = manyTill (try $ Normal <$> (many1 $ noneOf "#~")
          <|> try ( IShell <$>  (string "###" *> (lexeme $ many1 (noneOf " #\t\n\r")))
                    <*>  lexeme ( try $ many (try $ many1 $ lexeme (noneOf " \t\n\r#"))
                             <|> pure []) <* string "###")
          <|> try ( DMath <$> (bt "##" $ many1 (noneOf "#")))
          <|> try ( Math <$> (bt "#" $ many1 (noneOf "#")))
          <|> IText <$> (bt "~" $ many1 (noneOf "#~"))) (eof *> pure [])

-- raw document format (title,author,content)
data Doc = Doc String String [Bullet]
  deriving (Eq,Show)

parseHead :: String -> Parser String
parseHead hd = string hd *> many (noneOf "\n")

parseBull :: Char -> Parser Bullet
parseBull b = ( , ) <$> (ntabs 0 <* char b) <*> manyTill anyChar --(noneOf [])
              (lookAhead $ try (many1 (char '\t') <* char b ) <|> eof *> string "" )
  where ntabs n = try (char '\t' *> ntabs (n+1)) <|> pure n

parseDoc :: Parser Doc
parseDoc = Doc <$> (parseHead "TITLE:"  <* string "\n")
               <*> (parseHead "AUTHOR:" <* string "\n")
               <*> (manyTill (parseBull '-') (eof *> pure []))

-- decalre header-specific variables based on title and author
-- TODO: don't hardcode author/title variables
varDecs :: Doc -> String
varDecs (Doc title author _) = printf "\\newcommand{\\mytitle}{%s}\
  \ \n\\newcommand{\\myauthor}{%s}\n" title author

-- emit laTex from a document
-- TODO: have it return either rather than ad-hoc error throwing
emitDoc :: Doc -> IO String
emitDoc (Doc title author bullets) = do
  title' <- return  $ printf "\\title{%s}" title
  author' <- return $ printf "\\author{%s}" author
  bullets' <-  (emitBT True . conv) $ map lTextofBullet bullets
  return $ title' ++ "\n" ++ author' ++ "\n\\begin{document}\n\\maketitle\n" 
                  ++ bullets' ++ "\n\\end{document}"
  where lTextofBullet :: Bullet -> (Int,[LText])
        lTextofBullet (n,tex) = (n,rightOf $ parse parseLText "" tex)
        rightOf = \case Right a  -> Normal "\\item " : a
                        Left err -> error  $ "some sort of parse error happened" ++ show err

-- intermediate data structure to hold bullets
-- it has [list of bullets], optional children, optional continuation
-- of bullets at same level after children, so It's a binary tree
data  BT a = Node [a] (Maybe (BT a)) (Maybe (BT a))
  deriving (Eq,Show,Functor)

-- convert an assoc-list into a BT,
-- the assoc-list has the semantics of 'Indentation Levels' for bullets
conv :: Eq a => [(Int,a)] -> BT a
conv = \case
  []           -> Node [] Nothing Nothing
  xs@((n,_):_) -> Node (map snd curr)
      (if (child == []) || (fst $ head child) < n
         then Nothing else Just $ conv child)
      (if (next == []) then Nothing else Just $ conv next)
    where (curr,child,next) = split3 ((== n).fst) xs
          split3 f ls = (takeWhile f ls,takeWhile (not.f) mid,end)
            where mid = dropWhile f ls
                  end = dropWhile (not.f) mid

-- convert BT [LText] to well-itemized Latex;
-- Boolean flag indicates whether or not to begin with a \begin{itemize}
emitBT :: Bool -> BT [LText] -> IO String
emitBT b (Node ls next cont) = do
  bullets  <- concatM2 stringOfLText ls
  children <- case next of {Nothing -> return ""; Just s -> emitBT True s}
  continue <- case cont of {Nothing -> return "\\end{itemize}\n"; Just s -> emitBT False s}
  return $ (if b then "\\begin{itemize}\n" else "") ++ bullets ++ children ++ continue
  where concatM2 :: (LText -> IO String) -> [[LText]] -> IO String
        concatM2 f lls = (mapM (mapM f) lls)
          >>= (\ x -> return $ (concat . concat) x)
