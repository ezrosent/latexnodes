module AST where
import Control.DeepSeq

type Bullet = (Int,String) -- (level -> content)

-- definitions of LaTeX tokens
data LText = Normal String | Bold String | Underline String | IText String
  | IVShell String [String] -- inline shell output in verbatim block
  | IShell String [String]  -- inline output of shell command
  | Code String String -- language, text
  | LBlock String [LText] -- command, body
  deriving (Eq, Show)
instance NFData LText

-- Overall document: Title, Author, doc elements, bullets
data Doc = Doc String String [([DocElts],[Bullet])]
  deriving (Eq,Show)

data DocElts = Section String
  | SubSection String | SubSubSection String | Paragraph String
  deriving (Eq, Show)

-- intermediate data structure to hold bullets
-- it has [list of bullets], optional children, optional continuation
-- of bullets at same level after children, so It's a binary tree
--  so if we have:
--         a.
--            b.
--            c.
--         d.
--           e.
--             f.
--         g.
-- A -> (b -> c),(d->(e->f),g)
-- Where lone letters with no arrows after them are None
-- (same goes for letters pointing to only 1, rather than 2 children)
data  BT a = Node [a] (Maybe (BT a)) (Maybe (BT a))
  deriving (Eq,Show)
