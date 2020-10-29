{-# LANGUAGE TypeFamilies #-}

import Control.Monad (void)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Debug.Trace

{- Datový typ na tokeny. TWhitespace ukládá velikost mezery (kvůli měřitelnosti
 - zarovnání). -}
data Tok
  = TInt Int
  | TVar String
  | TWhitespace Int
  | TNewLine
  deriving (Show, Eq, Ord)

isNewLine (T _ TNewLine) = True
isNewLine _ = False

showTok :: Tok -> String
showTok = show -- tahle funkce tokeny zobrazuje v chybových hláškách typu
               -- "Unexpected TNewLine". Jestli chcete hezčí chybové hlášky, je
               -- vhodné ji upravit (např. aby se zobrazilo spíš lidštější
               -- "unexpected line end").

{- Kvůli hezkým chybovým hláškám z druhé úrovně parsování budeme potřebovat
 - rekonstruovat originální vstup (aby do něj pak šlo ukázat šipkou na chybu).
 - To není úplně nutné, ale proč to nezkusit -- typ T bude jednoduchá obálka
 - tokenu, která "nese" informaci o původním textovém tvaru:
 -
 - T "a" (TVar "a")
 - T "0x123" (TInt 291)
 - T "\"a s d\"" (TString "a s d")
 - T "\t\t  " (TWhitespace 18)
 -}
data T a =
  T String
    a
  deriving (Show)

unT (T _ a) = a --zahodí obal

strT (T s _) = s --vrátí původní string


{- PRVNÍ ÚROVEŇ PARSOVÁNÍ -}

type Tokenizer = Parsec Void String

{- Následuje hlavní funkce na parsování tokenů (tu asi budete chtít rozšířit).
 -
 - Data musíme ukládat dvakrát, jednou jako String (pro obal T) a podruhé
 - "naparsované" jako Tok; pro zjednodušení kódu můžeme použít Reader
 - následovně:
 -
 - (T <$> id <*> TVar) <$> some lowerChar
 -
 - ...je ekvivalentní
 -
 - do str <- some lowerChar
 -    return (T str (TVar str))
 -}
tok :: Tokenizer (T Tok)
tok =
  choice
    [ try ((T <$> id <*> TInt . read) <$> some digitChar)
    , (T <$> id <*> TVar) <$> some lowerChar
    , (T <$> id <*> TWhitespace . length) <$> some (char ' ')
    , T "\n" TNewLine <$ char '\n'
    ]

toks :: Tokenizer [T Tok]
toks = many tok

newtype TokStream = TokStream {unTokStream :: [T Tok]} deriving Show

tokenize = runParser (TokStream <$> toks) --tohle spustí tokenizér


{- DRUHÁ ÚROVEŇ PARSOVÁNÍ (syntaktická analýza)
 -
 - Pro příklad parsujeme primitivní jazyk z párů proměnných a čísel oddělených
 - nějakými mezerami.
 -
 - Protože na vlastní tokeny nejde použít písmenové nástroje z megaparsecu,
 - musíme použít "primitivnější" funkce:
 -  - satisfy (namatchuje jeden token splňující nějakou podmínku)
 -  - single (namatchuje libovolný token)
 -}
type Parser = Parsec Void TokStream --parsujeme seznam otagovaných tokenů

pWhite :: Parser () --tohle naparsuje jeden whitespacový token
pWhite = (void (single TNewLine) <|>
          void (satisfy isWhitespace)) <?> "whitespace"
  where
    isWhitespace (TWhitespace _) = True
    isWhitespace _ = False

whitespace = void $ many (try pWhite) -- libovolné množství whitespacu

pInt :: Parser Int -- jeden integer
pInt = do
  TInt i <- satisfy isInt <?> "an integer" -- label pro chybové hlášky, vznikne:
  return i                                 -- "expecting an integer".
  where
    isInt (TInt _) = True
    isInt _ = False

pVar :: Parser String -- jedna proměnná
pVar = do
  TVar s <- satisfy isVar <?> "a variable name"
  return s
  where
    isVar (TVar _) = True
    isVar _ = False

pLexeme :: Parser a -> Parser a
pLexeme = (whitespace >>)

pair = (,) <$> pLexeme pVar <*> pLexeme pInt

pairs = many pair <* whitespace

parseExprs = runParser (pairs <* eof)


{- MAIN
 - 
 - Finální main budete muset rozšířit o následující:
 - 1. načtení jména vstupního souboru
 - 2. načtení stringu ze souboru
 - 3. spuštění tokenizéru
 - 4. vyrobení blokových značek kolem bloků kódu, např. z kódu:
 -    
 -    a
 -      b
 -
 -    ...který ztokenizujete na:
 -    [T "a" (Var "a"),
 -     T "\n" TNewLine,
 -     T "  " (TWhitespace 2),
 -     T "b"]
 -
 -    ...budete chtít vyrobit:
 -
 -    [T "a" (Var "a"),
 -     T "\n" TNewLine,
 -     T "  " (TWhitespace 2),
 -     T "" TBlockStart,
 -     T "b",
 -     T "" TBlockEnd]
 -
 - 5. spuštění parseru
 - 6. naformátování (a vypsání) výsledku pomocí Text.PrettyPrint
 -}
main = do
  let Right tokens = tokenize "input.txt" "a 1 \n   b 5"
  putStrLn "*** Výstup tokenizéru: ***"
  print tokens

  let Right exprs = parseExprs "input.txt" tokens
  putStrLn "\n*** Výstup parseru: ***"
  print exprs
  
  let Left err =
        parseExprs
          "input.txt"
          (TokStream $ [T "123" (TInt 123), T " " (TWhitespace 1)]
                       ++ unTokStream tokens)
  putStrLn "\n*** Ukázka chybové hlášky: ***"
  putStrLn $ errorBundlePretty err
  
  let Left err =
        parseExprs
          "input.txt"
          (TokStream $ unTokStream tokens
                       ++ [T " " (TWhitespace 1), T "123" (TInt 123)])
  putStrLn "*** Lepší ukázka chybové hlášky (číslo řádku!): ***"
  putStrLn $ errorBundlePretty err
  
  let Right tokens = tokenize "input.txt" "a \n\n\n 1  b b  b\n5"
  putStrLn "*** Tokeny pro příští ukázku: ***"
  print tokens
  
  let Left err = parseExprs "input.txt" tokens
  putStrLn
    "*** Ještě lepší ukázka chybové hlášky 3 (s ukázkou chybného vstupu): ***"
  putStrLn $ errorBundlePretty err


{- Zbytek kódu obsahuje instanci Stream [T Tok], která funguje jako adaptér
 - mezi seznamy označkovaných tokenů a Megaparsecem. Pro String, Text a
 - ByteStringy existují podobné adaptéry. -}
instance Stream TokStream where
  type Token TokStream = Tok -- typ tokenu který uvidí parsovací funkce
  type Tokens TokStream = [Tok] -- typ pro segment tokenů
  tokenToChunk _ = (: []) -- nějaké konverzní funkce pro předchozí typy
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null
  take1_ (TokStream (x:xs)) = Just (unT x, TokStream xs) -- extrahuje první token (bez tagu)
  take1_ _ = Nothing
  takeN_ n (TokStream l@(_:_)) =
    Just (map unT $ take n l, TokStream $ drop n l) -- tosamé pro chunk
  takeN_ _ _ = Nothing
  takeWhile_ f (TokStream l) =
    (map unT $ takeWhile (f . unT) l, TokStream $ dropWhile (f . unT) l)
{- Sem dám velmi tlustou čáru, pod ní číst už asi nemusíte (nechcete)
 -
 - *************************************************************************
 -
 - Zbytek instance existuje jen kvůli hezčímu vypisování chybových hlášek.
 - Konkrétně, z tokenů (z tagů z T) rekonstruuje smysluplný kus původního kódu
 - (v našem případě řádek) a řekne megaparsecu, kde přesně se v něm nachází
 - problematický token (aby šel podtrhnout). Pro základní funkcionalitu je to
 - samozřejmě naprosto zbytečné, ale schopnost rozumně ukazovat chyby v kódu (a
 - ne jen na seznamu tokenů) je jedna ze základních vlastností dobrých parserů
 - a měla by být všude. -}
instance VisualStream TokStream where
  showTokens _ (a :| b) = intercalate ", " $ map showTok (a : b)

instance TraversableStream TokStream where
  reachOffset o pst =
    let oo = pstateOffset pst
        otoks = unTokStream $ pstateInput pst
        lineEnds =
          filter (isNewLine . snd) $
          takeWhile ((<= o) . fst) $ zip [oo ..] otoks
        newo
          | null lineEnds = oo
          | otherwise = succ . fst . last $ lineEnds
        newtoks = drop (newo - oo) otoks
        line =
          case concatMap strT $ takeWhile (not . isNewLine) newtoks of
            "" -> "<empty line>"
            a -> convertTabs a
              where convertTabs = concatMap convertTab
                    convertTab '\t' = replicate (unPos $ pstateTabWidth pst) ' '
                    convertTab c = [c]
        sp = pstateSourcePos pst
        srcLine = mkPos $ unPos (sourceLine sp) + length lineEnds
        lineo = o - newo
        srcCol = mkPos . (+ 1) . length . concatMap strT . take lineo $ newtoks
        newSrcPos = sp { sourceLine = srcLine
                       , sourceColumn = srcCol }
     in ( Just line
        , pst
            { pstateInput = TokStream newtoks
            , pstateOffset = newo
            , pstateSourcePos = newSrcPos
            })
