module Main where
import LatexNodes (emitDoc,varDecs)
import Parser (parseDoc)
import Text.ParserCombinators.Parsec (parse)
import Text.Printf (printf)
import System.Environment

doIO :: String -> String -> String -> IO ()
doIO proLF texF outFile = do
  p     <- readFile proLF
  txt   <- readFile texF
  txt'  <- case parse parseDoc "" txt of
             Left e -> error $ "illegal parse:\n" ++ show e
             Right d -> return d
  t     <- emitDoc txt'
  let h = varDecs txt' in
      writeFile outFile (printf "%s\n%s\n%s\n" h p t)

-- TODO: sane and helpful command-line arg stuff
main :: IO ()
main = do {[p,t,o] <- getArgs; doIO p t o}
