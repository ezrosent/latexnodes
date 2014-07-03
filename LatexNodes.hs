{-# LANGUAGE LambdaCase,TupleSections,MultiWayIf #-}
module LatexNodes where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), optional, many)
import Text.Printf (printf)
-- TODO: get nesting to work, then tables

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
        braces  = between (char '#') (char '#')

-- standard test thing for ghci
ptest :: Show a => Parser a -> String -> IO ()
ptest a s = print $ parse a "" s

stringOfLText :: LText -> String
stringOfLText = \case Math s   -> printf "\\( %s \\)" s
                      DMath s  -> printf "\\[ %s \\]" s
                      Normal s -> s
data BullTree = BT [[LText]] (Maybe BullTree)
-- a list of these things is a list of bullets, the next tree down, and
-- the continuation of the previous tree that was being build up


-- must convert to some tree datastructure.
  -- Algorithm: combine all adjascent ones of same depth
  -- when getting to n+1, return combine of all next level
  -- when getting to n-1, return current
  -- otherwise return error
  -- currently this doesn't pass the extra bits back up, which it needs
  -- to


ebt :: [(Int,[LText])] -> BullTree
ebt [] = BT [] Nothing
ebt lst@((n,_):_) = let (yes,no) = split ((== n).fst) lst in
                          BT (snd . combine $ yes) (disp no)
  where split f ls = (takeWhile f ls,dropWhile f ls)
        combine = (n,).(map snd)
        disp = \case []              -> Nothing
                     inls@((n',_):_) -> if | n' < n      -> Nothing
                                           | n' == (n+1) -> Just $ ebt inls
                                           | n' == n     -> error "programmer error"
                                           | otherwise   -> error "jumpted too far ahead"

emitDoc :: Doc -> String
emitDoc (Doc title author bullets) = printf "%s\n%s\n\\begin{document}\
  \ \n \\maketitle\n%s\n\\end{document}" title' author'  bullets'
  where title'   = (printf "\\title{%s}" title)   :: String
        author'  = (printf "\\author{%s}" author) :: String
        {-bullets' = concatMap (emitLT  . lTextofBullet) bullets-}
        bullets' = emitBullet $ map lTextofBullet bullets
        emitLT :: (Int,[LText]) -> String
        emitLT (n,lts)   = printf "\n%s\n%s\n%s\n" (flatten' "\\begin{itemize}") lts' (flatten' "\\end{itemize}")
          where lts'     = concatMap stringOfLText lts
                flatten' = (concatMap id) . (replicate n)
        lTextofBullet :: Bullet -> (Int,[LText])
        lTextofBullet (n,tex) = (n,rightOf $ parse parseLText "" tex)
        rightOf = \case Right a  -> Normal "\\item " : a
                        Left err -> error  $ "some sort of parse error happened" ++ show err
data  BT a = B [a] (Maybe (BT a)) (Maybe (BT a))
  deriving (Eq,Show)

test :: [(Int,String)]
test = [(1, "Hello"), (2, "There"), (3, "Sir"), (2, "How"), (1, "Dy"), (2,"Wat"),
        (2, "A"), (3, "Find"), (3, "Down"), (1,"Here")]

conv :: Eq a => [(Int,a)] -> BT a
conv = \case
  []           -> B [] Nothing Nothing
  xs@((n,_):_) -> B (map snd curr)
                    (if (child == []) || (fhd child) < n then Nothing
                             else Just $ conv child)
                    (if (next == []) then Nothing else Just $ conv next)
    where fhd = fst . head
          (curr,child,next) = split3 ((== n).fst) xs
          split3 f ls = (spl takeWhile,takeWhile (not.f) mid,end)
            where spl g = g f ls
                  mid = spl dropWhile
                  end = dropWhile (not.f) mid
-- not quite right with the begins and ends
emitBT :: Bool ->  BT [LText] -> String
emitBT b (B ls next cont)  = (if b then "\\begin{itemize}\n" else "")++
                          bullets ++  children  ++ continue
  where bullets = concatMap id $ map (concatMap $ (++ "\n") .  stringOfLText) ls
        children = case next of {Nothing -> ""; Just s -> emitBT True s}
        continue = case cont of {Nothing -> "\\end{itemize}\n"; Just s -> emitBT False s}

emitBullet :: [(Int,[LText])] -> String
emitBullet = emitBT True . conv
