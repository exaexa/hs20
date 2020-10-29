
# Úkol 2 -- od-odsazovač kódu

## Mírný úvod a předběžnosti

Na přednášce jsme řešili monadické parsování a parsery, podobné můžete najít v balících [`parsec`](), [`megaparsec`]() a [`attoparsec`](). Po rámcovém úvodu dostupném ve slajdech a v příkladech kódu na webu si můžete projít i nějaké (přesnější) tutorialy a ukázkový kód; nějaké jsou třeba [tady](https://markkarpov.com/learn-haskell.html#megaparsec-tutorials), konkrétně [tenhle](https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html) je zadání dnešního úkolu dost blízko. Detaily se dají sehnat z [dokumentace Megaparsecu](https://hackage.haskell.org/package/megaparsec).

V úkolu navíc budeme řešit zpracování a formátování textu, s tím souvisí pretty-printing. O tom na přednášce budeme teprve mluvit (odhadem na šesté až sedmé), zatím se může hodit poměrně jednoduchá knihovna na pretty-printing prakticky čehokoliv, [dostupná zde](https://hackage.haskell.org/package/pretty). Místo dokumentace vám možná může stačit jednoduchý [příklad zde](ppscheme.hs), který pěkně vypisuje a zarovnává Scheme.

## Vstup domácího úkolu

Jazyk Slepýš (slovensky Slepúch, v anglicky mluvících zemích by to byl asi
Blindworm) dovoluje následující konstrukce:

- výrazy, tvořené numerickými literály (jako `-1`, `0` or `0123`) a stringovými
  literály (`"asad"` or `"asda\nasda"`), jmény proměnných která se skládají z
  písmen `a`-`z`, `A`-`Z` a podtržítka `_`, a jakýchkoliv kombinací předchozího
  pomocí binárních operátorů `+`, `-`, `*`, `/`, `<`, `>` a závorek `(` a `)`.
- volání funkcí, které jsou zapsané "Cčkově" pomocí závorek a čárek, např.
  `asd(1,2,345)`
- příkazů (každý příkaz obsahuje jeden výraz na samostatném řádku)
- přiřazení (jméno proměnné následované `=` a výrazem, taky na samostatném řádku)
- výrazy `if`, `if`+`else` a `while`, podobně jako v konkurenčním hadovitém jazyce (s dvojtečkami)
- definice funkcí pomocí klíčového slova `def`, taky jako v pythonu, ale jen s
  obyčejnými argumenty (tj. bez keywordových argumentů nebo defaultních hodnot)

## Úkol, část první (prettyprinting)

Použijte buď `parsec` nebo `megaparsec` na zkonvertování Slepýšského kódu na
nějakou dočasnou reprezentaci (asi AST), ten následně vypište mírně upravený
pomocí knihovny `pretty` (nebo ručně, pokud se vám do toho chce).

Vstupní (ošklivý slepýšový) kód přečtěte ze souboru specifikovaném v jediném
argumentu z příkazové řádky, výstupní (hezčí) kód vypište na standardní výstup.

Hezčí kód by měl vypadat následovně:

- Syntax příkazů `if`, `while`, bloků kódu a definic funkcí je zkonvertovaná na
  Cčkovou, tj. kolem podmínek přidejte závorky, a kolem bloků kódu explicitné
  složené závorky `{` a `}`. Mezi příkazy přidejte středníky, pro definice
  funkcí zachovejte klíčové slovo `def`. Výsledný kód by neměl být závislý na
  zarovnání. (To mimo jiné znamená, že výsledný jazyk nebude validní slepýšový
  kód, ale bude bližší Cčku. Takto zkrášlený program samozřejmě nejde parsovat
  znova.)
- Kolem všech operátorů přidejte mezery; podobně přidejte mezery kolem závorek
  (ideálně jen na "vnější straně" závorek"), kromě volání funkcí (tj.
  nepřidávejte mezeru mezi jméno volané funkce a volací závorku).
- Kolem definic funkcí přidejte prázdné řádky
- Zbytečný whitespace (zbytečné prázdné řádky, nadbytečné mezery) odstraňte.
  Odsazení sloužící k optické přehlednosti kódu zarovnejte na 2 mezery pro
  každý blok.
- Neplatný kód odmítněte (ideálně s nějakou srozumitelnou chybovou hláškou).
- Můžete si vybrat, jestli vstupní zarovnání bude provedené pomocí tabů nebo
  mezer. Váš program by měl druhou možnost konzistentně odmítat. Pokud možno,
  preferujte mezery.

Například, soubor `test.slepys`:

```
def test(x): print(x+x)
i=0
j=read()
while i<j:
   test(i)
   i=i+1
```

...by měl jít zpracovat vaším programem např. takto:

```
slepys-format test.slepys
```

Výstup může vypadat následovně:
```
def test(x) {
  print(x + x);
}

i = 0;
j = read();
while (i < j) {
  test(i);
  i = i + 1;
};
```

Nejednoznačné a nedůležité jevy (mezeru za `if`/`while`, středník za blokem, ...) vyřešte libovolným způsobem.

Pro protipříklad, každý řádek následujícího programu obsahuje několik důvodů, kvůli kterým by ho vaše řešení mělo odmítnout zpracovat:

```
def a:
def ():
123asd = asd321 + + - 321
else: pass
if < a: xxx
```

## Úkol, část druhá (kontrola správnosti)

Po naparsování kódu zkontrolujte, jestli jsou všechny použité proměnné
definované (tj., jestli je identifikátor buď zachycený v parametrech funkce,
nebo přiřazený v nějakém předchozím výrazu, nebo je to jméno definované
funkce). Pokud v programu najedete takovou chybu, místo výstupu vypište
chybovou hlášku obsahující seznam jmen všech nedefinovaných proměnných, včetně mírné "navigace" která ukazuje na konkrétní část kódu ve které se chyba nachází.

Identifikátory `print`, `read` a `pass` jsou ve standardní Slepýšové knihovně, tj. jsou definované automaticky na začátku kódu.

Například, následující kód:

```
d = "asd"
def a(b):
  c = b+d+e
print(a(c))
```

by měl vyprodukovat chybovou hlášku rámcově podobnou této:

```
Error: Undeclared identifier 'e'
  .. found in assignment
     'c = b + d + e'
  .. found in definition of function 'a'
Undeclared identifier 'c'
  .. found in statement
     'print(a(c))'
```

Hint: na průchod AST s nějakým "kontextem" se dost hodí použít nějakou
kombinaci Readeru, Writeru nebo State monády, abyste se o kontext starat
ideálně nemuseli.

Zjednodušení: Slepýš neumí rekurzi, takže rekurzivní funkce a/nebo funkce,
které referují na funkce a proměnné definované až dále v kódu, můžete považovat
za chybné. Např.:
```
def a():
  a()

a()
```
...si může stěžovat na nedefinované `a` v definici funkce `a`.

##### Nedůležitá poznámka o nechybějící rekurzi

Slepýš sice rekurzi neumí, ale to nám nebrání, abychom ji dodefinovali ručně
pomocí fixed-point operátoru!

```
def faktorial(faktorial_rec):
  def f(a):
    if a>1:
      a*faktorial_rec(a-1)
    else:
      1
  f

def fix(f):
  def f_omega(x): f(x(x))
  f_omega(f_omega)

print(fix(faktorial)(5))
```

# Obecné rady

Pokud nevíte, jak začít, [použijte tuhle kostru](minisnake.hs) 2-úrovňového
parseru primitivního jazyka, a expandujde ji na Slepýše. Pokud náhodou máte
megaparsec verze 8 nebo starší, použijte [starší verzi](minisnake-8.hs).

Doporučený postup řešení je následujcící:
- v první úrovni parsování kód rozbijte na obyčejné tokeny, whitespace na
  začátku řádku si uložte jako speciální token "Indent o N řádků".
- výsledný stream tokenů transformujte nějakou jednoduchou funkcí, která kolem
  skupin stejně zarovnaných řádků doplní umělé tokeny `IndentIn` a `IndentOut`
  (tj. zárodky budoucích `{` a `}`), případně prohlásí že program je špatně.
  (Ohraničení bloků se v různých jazycích jmenuje různě, v pythonu třeba
  `indent` a `dedent`.)
- Výsledný seznam tokenů můžete parsovat normálním parserem na AST, které je už
  celkem jednoduché zkontrolovat a vypsat.

## Vstup a výstup

Použijte
[getArgs](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Environment.html#v:getArgs)
a případně
[exitWith](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Exit.html#v:exitWith),
pomocí kterých můžete program donutit k rozumnému unix-ovitému chování. O
ostatní argumenty (např. o očekavatelný `--help`) se nestarejte, ale zajistěte,
že program nespadne pokud s dodaným souborem něco nebude v pořádku a nepůjde
přečíst (např. pomocí nějakých funkcí na chytání IO errorů).

## Nejednoznačnosti specifikace jazyka

S jednoznačností definice bloků pomoci zarovnání je obecně dost potíží, nějaké
si můžete ušetřit např. tím, že následující kód:

```
if a: b
      c
      d
```

budete parsovat jako:

```
if a: {
  b;
};

{
  c;
  d;
};
```

## Nějaké užitečné monády

### Použití parserů z Megaparsecu

Typ `Parser a` ze slajdů není v Parsecu/Megaparsecu přímo k dispozici, místo
toho si typ většinou definujete ručně z typů chyb a proudu, který chcete
parsovat:

```hs
type Parser = Parsec Void String
```

...říká, že chybové reporty parser umět nebude (`Void`), a že vstupem parseru
jsou prvky proudu `String` (tj. chary).S touto definicí je `Parser Int`
víceméně stejný typ jako parser integerů `Parser Int` ze slajdů.

V případě dvou úrovní parsování je poměrně běžné mít první parser ("tokenizér")
který text předělává na tokeny (tj. `Parsec Void String [Token]`) a druhý
parser který tokeny seskupuje do nějakých syntaktických skupin, tj. vyrábí AST
(`Parsec Void [Token] AST`). V prvním případě se můžou dost hodit utility pro
zpracování písmen z
[Text.Megaparsec.Char](https://hackage.haskell.org/package/megaparsec-7.0.5/docs/Text-Megaparsec-Char.html)
a
[Text.Megaparsec.Char.Lexer](https://hackage.haskell.org/package/megaparsec-7.0.5/docs/Text-Megaparsec-Char-Lexer.html)
(nebo jejich odpovídající varianty pro
[Parsec](https://hackage.haskell.org/package/parsec-3.1.14.0)). Na parsování
vlastního proudu tokenů budete potřebovat [generické funkce
odsud](https://hackage.haskell.org/package/megaparsec-7.0.5/docs/Text-Megaparsec.html#g:5).
Abyste vlastní proud mohli použít v Megaparsecu, je pro něj potřeba vyrobit
instanci odpovídající typové třídy, tu najdete v kostře řešení.

Dvojúrovňové parsování ale není nijak povinné. (Udělat to rozumně a
rozšiřitelně jinak je ale dost složité na to, abych to považoval za bonus.)

### Reader a Writer

`Reader` je monáda, která skrz program transparentně [předává nějakou
hodnotu](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html),
představte si ji jako `State` který jde jen číst. `Reader` budete pravděpodobně
chtít použít v druhé části úkolu na předávání seznamu dostupných proměnných.

Hodí se následující funkce (typy jsou zjednodušené):

- `runReader :: Reader env a -> env -> a` spustí výpočet (v prvním parametru) s
  globálním parametrem typu `env` (v druhém parametru) a vrátí výsledek výpočtu
- ve výpočtu je možné použít `ask :: Reader a a` k přečtení globálního stavu
  (podobně jako u `get :: State s s`)
- `local :: (env -> env') -> Reader env' a -> Reader env a` modifikuje
  "prostředí" pomocí funkce v prvním parametru, a spustí "lokální" výpočet s
  modifikovaným přostředím (druhý parametr). Okolní výpočet pokračuje s
  původním prostředím.

Například
```hs
f = do res <- local (*2) $ do a <- ask
                              return [a,a]
       return $ res++res

main = print $ runReader f 1
```
vypíše `[2,2,2,2]`.

`Writer` funguje přesně obráceně -- ukládá pologrupový "stav" určený pouze pro
zápis. Podpůrné funkce jsou podobné jako u readeru, ty důležité se jmenují
`runWriter` and `tell`. Writer se dost hodí pro "logování" chybových hlášek, na
které můžete narazit v první části úkolu.

```hs
w = do tell ["ahoj"]
       tell ["log 2"]
       tell ["log 3"]
       return 42

main = print $ runWriter w
```

Program vypíše `(42, ["ahoj","log 2","log 3"])`.

Kombinaci Readeru a Writeru můžete buď simulovat pomocí `State`, implementovat
manuálně (bonusové body!), použít trochu brutálnější knihovní implementaci
kombinace Reader+Writer+State
[RWS](https://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-RWS-CPS.html#t:RWS)
(trochu míň bonusových bodů), NEBO, v nejkorektnějším případě (za nejvíc
bonusových bodů!!!), nakombinovat ze základních monád pomocí transformerů,
např. jako `ReaderT env (Writer log) a`.

Implementace Readeru, Writeru, RWS a všemožných dalších zajímavostí jsou k
dispozici v knihovnách `mtl` (starší a občas strozumitelnější) a `transformers`
(novější, lepší a doporučená varianta, transformery jsou ale trochu temnější
protože místo víceparametrových typových tříd používají asociované typy). Pokud
můžete, používejte `transformers`.


# Submission

Z programu vyrobte cabalový balík pojmenovaný `u2-vaseprijmeni` (úplně stejně
jako v předchozích úkolech), zabalte ho pomocí `cabal sdist` a nahrajte na
správné místo do SISu. Balík by měl sestavit jediný program pojmenovaný
`slepys-format`.

Před odevzdáním zkontrolujte, že archiv je kompletní a funguje (hlavně pokud
zdrojový kód rozdělíte na víc souborů), a že jde spustit pomocí `cabal run`.

Tradičně doporučuju konzultovat `hlint` a kód si nechat automaticky zformátovat.
