# Úkol 1 -- robodungeon

Simulujme jeskyni obdélníkového půdorysu, která je popsatelná mapou vyrobenou ze čtverečkových políček. Na políčkách se jednotlivě vyskytují následující objekty:

- prázné místo
- stěna
- rychlý robot, natočený jedním ze 4 možných směrů
- pomalý robot, taky natočený jedním ze 4 směrů, navíc s jedním bitem paměti (viz. níže).

"Okolí" jeskyně (tj. věci mimo mapu) je pro pořádek vyplněné stěnou.

Čas v jeskyni ubíhá diskrétně po "tikách". Při každém tiku roboti provedou přesně jednu akci, která je následující:

- Rychlý robot jede o 1 políčko dopředu, pokud tam je prázdné místo. Pokud tam není prázdné místo, otočí se o 90 stupňů doleva.
- Pomalý robot se chová podle strategie "levá stěna", tj. snaží se jet podél zdi (pravděpodobně cyklicky). To provede následovně: Pokud je před ním stěna, otočí se doprava. Pokud před ním není stěna a vlevo od něj stěna je, jede rovně. Pokud vlevo od něj stěna chybí, otočí se doleva a zapamatuje si (do jednoho bitu paměti), že příští kolo musí jet rovně (pokud nenastanou zvláštní okolnosti, např. když na volné místo v dalším tahu vjede jiný robot).
- V případě kolize má přednost robot, který jede víc "shora" a "zleva".

( Chování robotů je inspirované poměrně starou hrou [Down-to-Earth](https://worldofspectrum.org/infoseekid.cgi?id=0001470). )

## Úkol

Vyrobte jednoduchou simulaci takové jeskyně. Simulátor by měl uživateli dovolit jezdit po mapě nějaké přednastavené velikosti kurzorem, ovládaným například pomocí šipek, a na pozici kurzoru umisťovat stěny nebo roboty, případně tyto objekty mazat (tj. "umisťovat" tam prázdné místo). Uživateln navíc nějakou speciální klávesou (např. mezerníkem) může nechat simulaci proběhnout o 1 časový tik dopředu, tj. roboti by se měli pohnout podle svých pravidel.

Jak nastavíte velikost mapy je jedno, ale simulátor by měl podporovat aspoň 10x10 políček.

## Návod

Použijte nějakou rozumnou Haskellovou knihovnu na výrobu jednoduchých grafických aplikací, doporučuju jednu z následujících:

- [Gloss](https://hackage.haskell.org/package/gloss-1.13.1.2/docs/Graphics-Gloss.html) -- dostanete jednoduché grafické okno do kterého můžete kreslit vektorovou grafiku, knihovna navíc podporuje podobné jednoduché skoro-hry pomocí funkce [`play`](https://hackage.haskell.org/package/gloss-1.13.1.2/docs/Graphics-Gloss.html#v:play).
- [Brick](https://github.com/jtdaugherty/brick/) -- knihovna účelem podobná Glossu, ale místo grafického okna pro vektorovou grafiku dostanete terminálové okno pro kreslení ASCII grafiky.

Grafickou reprezentaci robotů a ostatních objektů nepřehánějte, v případě Glossu stačí obarvený nasměrovaný trojúhelník, v případě Bricku tradiční ASCII mapa podobná např. nethacku (`.` je prázdno, `#` je stěna, `^` je robot mířící nahoru, typ robota můžete rozlišit barvou, kurzor podbarvením, ...).

Pro oba případy můžete využít jednoduché připravené aplikace, které "už něco dělají":

- [gloss.hs](./gloss.hs) -- potřebuje knihovnu `gloss`, tu si můžete nainstalovat pomocí `cabal install gloss`, zdrojový kód "mimo projekt" skompilujete pomocí `ghc -package gloss gloss.hs -o gloss`
- [brick.hs](./brick.hs) -- potřebuje knihovny `brick` a `vty`, po jejich nainstalování kompilujte pomocí `ghc -package brick -package vty brick.hs -o brick`

## Bonusy

- Celou mapu můžete reprezentovat jako seznam seznamů, ale mnohem lepší je použít arraye (přecejen jsou rychlejší...). Nízkoúrovňové funkcionální arraye z `Data.Array` jsou poněkud moc generické (tj. pro začátečníky nepoužitelné), proto je výrazně lepší je používat obalené v [`Data.Vector`](https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector.html). Vektor je v tomto případě víceméně ekvivalent konstantní C-čkové arraye.
- Vektory jsou `konstantní' a copy-on-write, tj. změna jednoho prvku někde uprostřed je docela drahá. Místo toho můžete zkusit jednu z následujících možností:
  - Vektor můžete nejdřív vyrobit jako seznam (to je díky lazy vyhodnocování většinou "zadarmo"), a na ten pak zavolat `fromList`. Kolize robotů nemusíte řešit chaoticky, ale pomocí mezikroku, ve kterém si vygenerujete "mapu rezervací".
  - Použít [`Data.Vector.Mutable`](https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector-Mutable.html) (to je víceméně obyčejná C-čková zapisovatelná array). Čtení a úpravy mutovatelného vektoru jsou ale operace, u kterých záleží na pořadí provádění, takže je nutné je "spojovat" opatrně, stejným mechanismem jako IO akce. [Příklad s bubblesortem](https://www.ksi.mff.cuni.cz/~kratochvil/haskell/source/MVectorBubbleSort.hs).

## Odevzdání a hodnocení

Z programu vyrobte cabalový balík pojmenovaný `u1-vaseprijmeni`, ten zabalte pomocí `cabal sdist` a nahrajte do odpovídající kolonky do SISu.

Snažte se o hezký, přehledný, čitelný a _krátký_ kód. Tj.: pokud možno neřešte všechny problémy "ručně" rekurzí, ale používejte rozumné vyšší funkce (`map`, `zip`, `fold`, ...). Opakovaný kód vytrhněte do vlastní funkce a nahraďte nějakou zkratkou (ve funkcionálním programování jde vytrhnout a zkrátit prakticky cokoliv).

Odchylky od specifikace (např. mírně jiné chování robotů nebo jinak vyřešené kolize, jinak vyřešené ovládání, úplně jiný interface, atd.) jsou v rozumné míře OK.
