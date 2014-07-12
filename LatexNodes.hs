{-# LANGUAGE LambdaCase,TupleSections,DeriveFunctor #-}
module LatexNodes where
import Text.ParserCombinators.Parsec
import System.Process
import Control.Applicative hiding ((<|>), optional, many)
import Text.Printf (printf)
import Control.Parallel.Strategies
import Control.DeepSeq

type Bullet = (Int,String) -- (level -> content)

-- definitions of LaTeX tokens
data LText = Normal String | Bold String | Underline String | IText String
  | IShell String [String]  -- inline output of shell command
  | IVShell String [String] -- inline shell output in verbatim block
  deriving (Eq, Show)
instance NFData LText

data DocElts = Section String
  | SubSection String | SubSubSection String | Paragraph String
  deriving (Eq, Show)

stringOfDocElt :: DocElts -> IO String
stringOfDocElt = \case
                    Section s -> return $ "\\section*{" ++ s ++ "}\n"
                    SubSection s -> return $ "\\subsection*{" ++ s ++ "}\n"
                    SubSubSection s -> return $ "\\subsubsection*{" ++ s++ "}\n"
                    Paragraph s -> do
                      lt <- case parse parseLText "" s of
                              Left e -> error $ "error found in paragraph found\n" ++ (show e)
                              Right r -> return r
                      mapM stringOfLText lt >>= return . (++ "\n\n") . concat

stringOfLText :: LText -> IO String
stringOfLText = \case Bold s        -> return $ "\\textbf{" ++ s ++ "}"
                      Underline s   -> return $ "\\underline{" ++ s ++ "}"
                      IText s       -> return $ "\\textit{" ++ s ++ "}"
                      Normal s      -> return s
                      IVShell s ls  -> readProcess s ls "" >>= 
                        (\r -> return $ "\\begin{verbatim}" ++ r ++ "\\end{verbatim}")
                      IShell s ls   -> readProcess s ls "" >>= return

-- flexible type definition for parsing, makes it more extensible later
-- on if not all LTexts are strings

bt :: String -> Parser a -> Parser a
bt b s = string b *> s <* string b
lexeme :: Parser a -> Parser a
lexeme p = (many $ oneOf  " \t\n\r") *> p <* (many $ oneOf " \t\n\r")

parseLText :: Parser [LText]
parseLText = manyTill (try  (Normal <$> (many1 $ noneOf "#~"))
          <|> try (IVShell <$>  (string "#v##" *> (lexeme $ many1 (noneOf " #\t\n\r")))
                    <*>  many (lexeme (try (many1 $ oneOf " \n\t\r") -- whitespace-separated args
                                  <|> (many1 $ noneOf " \n\t\r#"))) <* string "###")
          <|> try (IShell <$>  (string "###" *> (lexeme $ many1 (noneOf " #\t\n\r")))
                    <*>  many (lexeme (try (many1 $ oneOf " \n\t\r")
                                  <|> (many1 $ noneOf " \n\t\r#"))) <* string "###")
          <|> try (Underline <$> (bt "##" $ many1 (noneOf "#")))
          <|> try (Bold      <$> (bt "#"  $ many1 (noneOf "#")))
          <|> IText <$> (bt "~" $ many1 (noneOf "#~"))) $
              (try  (string "\n\n") <|> eof *> pure "") *> pure []

parseSection :: Parser [DocElts]
parseSection = manyTill section $ (lookAhead $ try $ many1 (char '\t') *> pure [])
  where section = try (Section <$> (string "Section:" *> (many1 (noneOf "\t\n\r")) <* char '\n'))
              <|> try (SubSection    <$>
                      (string "Sub:" *> (many1 (noneOf "\t\n\r")) <* char '\n'))
              <|> try (SubSubSection <$>
                      (string "Subsub:" *> (many1 (noneOf "\t\n\r")) <* char '\n'))
              <|> try (Paragraph     <$> (manyTill (noneOf "\t\r")  $ try (string "\n\n")))
-- raw document format (title,author,content)
data Doc = Doc String String [([DocElts],[Bullet])]
  deriving (Eq,Show)


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
                    (manyTill (parseBull '-') $ (try (eof *> pure [])
                                                <|> (string "\n\n") *> pure []))

-- declare header-specific variables based on title and author
-- TODO: don't hardcode author/title variables
varDecs :: Doc -> String
varDecs (Doc title author _) = printf "\\newcommand{\\mytitle}{%s}\
  \ \n\\newcommand{\\myauthor}{%s}\n" title author

-- emit laTex from a document
-- TODO: have it return either rather than ad-hoc error throwing
emitDoc :: Doc -> IO String
emitDoc (Doc title author bullets) = do
  title'    <- return $ printf "\\title{%s}" title
  author'   <- return $ printf "\\author{%s}" author
  rest'     <- mapM getEltPair bullets >>= (return . concat)
  return $ title' ++ "\n" ++ author' ++ "\n\\begin{document}\n\\maketitle\n" 
                  ++ rest' ++ "\n\\end{document}"
  where getEltPair :: ([DocElts],[Bullet]) -> IO String
        getEltPair (d,b) = do
          docElts' <- mapM stringOfDocElt d >>= (return . concat)
          bullets' <- (emitBT True . conv) $ map lTextofBullet b
          return $ docElts' ++ bullets'
        lTextofBullet :: Bullet -> (Int,[LText])
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
-- TODO: parallelize recursive calls, straightforward method does not
-- work
conv :: (Eq a,NFData a) => [(Int,a)] -> BT a
conv = \case
  []           -> Node [] Nothing Nothing
  xs@((n,_):_) -> Node (map snd curr)
      (if (child == []) || (fst $ head child) < n
         then Nothing else Just $ conv child)
      (if (next == []) then Nothing else Just $ conv next)
    where (curr,child,next) = split3' ((== n).fst) xs
          split3' f ls = runEval $ do
                          mid' <- rpar $ force (dropWhile f ls)
                          a'   <- rpar $ force (takeWhile f ls)
                          rseq mid'
                          b'   <- rpar $ force (takeWhile (not.f) mid')
                          c'   <- rpar $ force (dropWhile (not.f) mid')
                          rseq a'; rseq b'; rseq c';
                          return (a',b',c')

-- convert BT [LText] to well-itemized Latex;
-- Boolean flag indicates whether or not to begin with a \begin{itemize}
emitBT :: Bool -> BT [LText] -> IO String
emitBT b (Node ls next cont) = do
  bullets  <- (mapM (mapM stringOfLText) ls) >>= return . (concat . concat) -- convert to string, flatten
  children <- case next of {Nothing -> return ""; Just s -> emitBT True s}
  continue <- case cont of {Nothing -> return "\\end{itemize}\n"; Just s -> emitBT False s}
  return $ (if b then "\\begin{itemize}\n" else "") ++ bullets ++ children ++ continue
