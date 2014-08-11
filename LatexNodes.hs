{-# LANGUAGE MultiWayIf,LambdaCase,TupleSections,DeriveFunctor #-}
module LatexNodes where
import Text.ParserCombinators.Parsec
import System.Process
import Text.Printf (printf)
import Control.Parallel.Strategies
import Control.DeepSeq
import AST
import Parser (parseLText)

--wrap for simple latex expressions
(\>) :: String -> String -> String
str \> s = printf "\\%s{%s}\n" str s
(\\>) :: String -> String -> String
str \\> s = printf "\\%s{%s}" str s

stringOfDocElt :: DocElts -> IO String
stringOfDocElt = \case
  Section s       -> return $ "section*" \> s
  SubSection s    -> return $ "subsection*" \> s
  SubSubSection s -> return $ "subsubsection*" \> s
  Paragraph s -> do
    lt <- case parse parseLText "" s of
            Left e -> error $ "This paragraph:\n" ++
                      s ++ "\ncontains the following error:\n" ++ (show e)
            Right r -> return r
    mapM stringOfLText lt >>= return . (++ "\n\n") . concat

stringOfLText :: LText -> IO String
stringOfLText = \case
  Bold s        -> return $ "textbf" \\> s
  Underline s   -> return $ "underline" \\> s
  IText s       -> return $ "textit" \\> s
  Normal s      -> return s
  IVShell s ls  -> readProcess s ls "" >>=
    (\r -> return $ "\\begin{verbatim}" ++ r ++ "\\end{verbatim}")
  IShell s ls   -> readProcess s ls ""
  LBlock cmd body -> (mapM stringOfLText body) >>= return . concat
                      >>= return . (++ "end" \> cmd) . ("begin" \\> cmd ++)
  Code lang source -> return $ ("begin" \\> "minted") ++
      "[frame=single,mathescape]" ++ "{" ++ lang ++ "}" ++
      source ++ "\n" ++ ("end" \> "minted") ++ "\n"

-- declare header-specific variables based on title and author
-- TODO: don't hardcode author/title variables
varDecs :: Doc -> String
varDecs (Doc title author _ ) = printf "\\newcommand{\\mytitle}{%s}\
  \ \n\\newcommand{\\myauthor}{%s}\n" title author

-- emit laTex from a document
-- TODO: have it return either rather than ad-hoc error throwing
emitDoc :: Doc -> IO String
emitDoc (Doc title author bullets) = do
  title'    <- return $ "title" \> title
  author'   <- return $ "author" \> author
  rest'     <- concatmapM getEltPair bullets
  return $ title' ++ "\n" ++ author' ++ "\n\\begin{document}\n"  -- deleted \maketitle
                  ++ rest' ++ "\n\\end{document}"
  where concatmapM f l = mapM f l >>= return . concat
        getEltPair :: ([DocElts],[Bullet]) -> IO String
        getEltPair (d,b) = do
          docElts' <- concatmapM stringOfDocElt d
          bullets' <- (emitBT True . conv) $ map lTextofBullet b
          return $ docElts' ++ bullets'
        lTextofBullet :: Bullet -> (Int,[LText])
        lTextofBullet (n,tex) = (n,rightOf tex $ parse parseLText "" tex)
        rightOf t = \case Right a  -> Normal "\\item " : a
                          Left err -> error  $ "parse error on input:\n" ++ (t) ++
                            "\nyielded the following error:\n" ++ show err


-- convert an assoc-list into a BT,
-- the assoc-list has the semantics of 'Indentation Levels' for bullets
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
