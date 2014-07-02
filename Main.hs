module Main where
import LatexNodes (parseDoc,emitDoc)
import Text.ParserCombinators.Parsec

stringDoc :: String -> String
stringDoc s = case parse parseDoc "" s of
                Left e -> show e
                Right d -> emitDoc d

main :: IO ()
main = print $ stringDoc ("TITLE: Test\n"
                                  ++ "AUTHOR: Yo There\n"
                                  ++ "\t\t\t- hello\n"
                                  ++ "\t- {there}\n"
                                  ++ "\t\t- sir\n")
