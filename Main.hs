module Main where
import LatexNodes (parseDoc,emitDoc,varDecs)
import Text.ParserCombinators.Parsec (parse)
import Text.Printf (printf)
import Control.Applicative
import System.Environment

doIO :: String -> String -> String -> IO ()
doIO proLF texF outFile = do
  p     <- readFile proLF
  (t,h) <- stringDoc <$> readFile texF
  writeFile outFile (printf "%s\n%s\n%s\n" h p t)
  where stringDoc s = case parse parseDoc "" s of
                        Left e -> error $ "illegal parse:\n" ++ show e
                        Right d -> (emitDoc d,varDecs d)

-- TODO: sane and helpful command-line arg stuff
main :: IO ()
main = do {[p,t,o] <- getArgs; doIO p t o}
