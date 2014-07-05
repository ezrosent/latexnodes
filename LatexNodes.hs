{-# LANGUAGE ExistentialQuantification,GADTs,LambdaCase,TupleSections,DeriveFunctor #-}
module LatexNodes where
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), optional, many)
import Text.Printf (printf)

type Bullet = (Int,String) -- (level -> content)

data LText = Normal String | Math String | DMath String | IText String
  deriving (Eq,Show)
-- BEGIN: GADT tests
data LT a where
  Norm :: String      -> LT String
  Mat  :: [LT String] -> LT [LT String]
  DMat :: [LT String] -> LT [LT String]
  ITex :: String      -> LT String

-- needed an existential wrapper to get this all to work
-- need to read more on haskell GADTs
data DLT where
  DLT :: forall a. LT a -> DLT


parseLTs :: Parser (LT String)
parseLTs =  (try $ (Norm <$> (many1 $ noneOf "#~"))
            <|> (ITex <$> bt "~" (many1 $ noneOf "#~")))

parseLTm :: Parser (LT [LT String])
parseLTm = (try (ltp DMat "##") <|> (ltp Mat "#")) 
  where ltp c s = c <$> (bt s $ many parseLTs )

stringOfLT :: LT a -> String
stringOfLT = \case Norm s -> s
                   ITex s -> s
                   Mat  l -> printf "\\( %s \\)" $ concatMap stringOfLT l
                   DMat l -> printf "\\[ %s \\]" $ concatMap stringOfLT l

parseLT :: Parser [DLT]
parseLT = manyTill parseAll (eof *> pure [])
  where parseAll = try (DLT <$> parseLTs) <|> (DLT <$> parseLTm)

stringOfDLT :: DLT -> String
stringOfDLT (DLT a) = stringOfLT a

injectLTs :: LT String -> LText
injectLTs = \case
               Norm s -> Normal s
               ITex s -> IText s
injectLTm :: LT [LT String] -> LText
injectLTm = \case
               Mat  l  -> Math  $ concatMap stringOfLT l
               DMat l  -> DMath $ concatMap stringOfLT l

injectLT :: LT a -> LText
injectLT = \case
              Norm s  -> Normal s
              ITex s  -> IText s
              Mat  l  -> Math  $ concatMap stringOfLT l
              DMat l  -> DMath $ concatMap stringOfLT l

--END: GADT tests
-- flexible type definition for parsing, makes it more extensible later
-- on if not all LTexts are strings
data LParse where
  LP :: forall a. (a -> LText) -> Parser a -> LParse 
bt :: String -> Parser a -> Parser a
bt b s = string b *> s <* string b
-- definitions of LaTeX tokens
pList' :: [LParse]
pList' = [
  LP Normal (many1 $ noneOf "#~"),
  LP DMath  (bt "##" $ many1 (noneOf "#")),
  LP Math   (bt "#" $ many1 (noneOf "#")),
  LP IText  (bt "~" $ many1 (noneOf "#~"))
  ]
pList :: [LParse]
pList = [
  LP injectLTs parseLTs,
  LP injectLTm parseLTm
  ]

parseLP :: [LParse] -> Parser [LText]
parseLP [] = pure []
parseLP (x:xs) = manyTill (lToken $  map token' xs) (eof *> pure [])
  where token' (LP f p) = f <$> p
        lToken = foldl (\ a b  -> (try a) <|> b) $ token' x

parseLText :: Parser [LText]
parseLText = parseLP pList

-- raw document format (title,author,content)
data Doc = Doc String String [Bullet]
  deriving (Eq,Show)

parseHead :: String -> Parser String
parseHead hd = string hd *> many (noneOf "\n")

parseBull :: Char -> Parser Bullet
parseBull b = ( , ) <$> (ntabs 0 <* char b) <*> manyTill anyChar --(noneOf [])
              (lookAhead $ try (many1 (char '\t') <* char b )<|> eof *> string "" )
  where ntabs n = try (char '\t' *> ntabs (n+1)) <|> pure n

parseDoc :: Parser Doc
parseDoc = Doc <$> (parseHead "TITLE:" <* string "\n")
               <*> (parseHead "AUTHOR:" <* string "\n")
               <*> (manyTill (parseBull '-') (eof *> pure []))


--- end parsing section
stringOfLText :: LText -> String
stringOfLText = \case Math s   -> printf "\\( %s \\)" s
                      DMath s  -> printf "\\[ %s \\]" s
                      IText s  -> printf "\\textit{%s}" s
                      Normal s -> s

-- decalre header-specific variables based on title and author
-- TODO: don't hardcode author/title variables
varDecs :: Doc -> String
varDecs (Doc title author _) = printf "\\newcommand{\\mytitle}{%s}\
  \ \n\\newcommand{\\myauthor}{%s}\n" title author

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
data  BT a = Node [a] (Maybe (BT a)) (Maybe (BT a))
  deriving (Eq,Show,Functor)

-- convert an assoc-list into a BT,
-- the assoc-list has the semantics of 'Indentation Levels'
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
emitBT :: Bool ->  BT [LText] -> String
emitBT b (Node ls next cont)  = printf "\
\%s%s%s%s"   (if b then "\\begin{itemize}\n" else "") bullets children continue
  where bullets  = concatMap (concatMap stringOfLText) ls
        children = case next of {Nothing -> ""; Just s -> emitBT True s}
        continue = case cont of {Nothing -> "\\end{itemize}\n"; Just s -> emitBT False s}
