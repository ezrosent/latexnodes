module Main where
import LatexNodes (parseDoc,emitDoc)
import Text.ParserCombinators.Parsec
import Control.Applicative
import Text.Printf (printf)
import System.Environment

stringDoc :: String -> String
stringDoc s = case parse parseDoc "" s of
                Left e -> show e
                Right d -> emitDoc d

doIO :: String -> String -> String -> IO ()
doIO proLF texF outFile = do
  p <- readFile proLF
  t <- stringDoc <$> readFile texF
  writeFile outFile (printf "%s\n%s\n" p t)

main :: IO ()
main = do {[p,t,o] <- getArgs; doIO p t o}

{-main = do-}
  {-[s] <- getArgs-}
  {-str <- readFile s-}
  {-putStrLn $ stringDoc str-}
{-
main = print $ stringDoc ("TITLE: Test\n"
                                  ++ "AUTHOR: Yo There\n"
                                  ++ "\t\t\t- hello\n"
                                  ++ "\t- ##there##\n"
                                  ++ "\t- #there#\n"
                                  ++ "\t\t- sir\n")
-}
