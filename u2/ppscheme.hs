import Text.PrettyPrint

-- jednoduchá reprezentace jazyka Scheme
data Scheme
  = Ident String
  | Number Int
  | Seq [Scheme]

-- třída na zkonvertování čehokoliv na Doc, což je "hezky vypsatelný dokument"
-- specifikovaný v modulu Text.PrettyPring
class PDoc a where
  pdoc :: a -> Doc

-- pomůcka na "renderování" čehokoliv renderovatelného do Stringu
ppshow :: PDoc a => a -> String
ppshow = renderStyle (style {lineLength = 80}) . pdoc

-- Scheme je v třídě PDoc, tj. můžeme ho vypsat hezky
instance PDoc Scheme where
  pdoc (Ident a) = text a
  pdoc (Number i) = int i
  pdoc (Seq []) = parens empty
  pdoc (Seq (x:xs)) = parens $ pdoc x <+> sep (map pdoc xs)

-- nagenerujeme trochu Schemového AST
short k = Seq $ Ident "*" : map Number [k .. k + 3]
long = Seq $ Ident "+" : map short [1 .. 10]

-- použití
main = putStrLn . ppshow $ Seq [Ident "factorial", long]
