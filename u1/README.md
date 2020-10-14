# Úkol 1 -- Robodungeon

Simulujme jeskyni obdélníkového půdorysu, která je popsatelná mapou vyrobenou ze čtverečkových políček. Na políčkách se jednotlivě vyskytují následující objekty:

- prázné místo
- stěna
- rychlý robot, natočený jedním ze 4 možných směrů
- pomalý robot, taky natočený jedním ze 4 směrů

"Okolí" jeskyně (tj. věci mimo mapu) je pro pořádek vyplněné stěnou.

Čas v jeskyni ubíhá diskrétně po "tikách". Při každém tiku roboti provedou přesně jednu akci, která je následující:

- Rychlý robot jede o 1 políčko dopředu, pokud tam je prázdné místo. Pokud tam není prázdné místo, otočí se o 90 stupňů doleva.
- Pomalý robot se chová podle strategie "levá stěna", tj. snaží se jet podél zdi (pravděpodobně cyklicky). To provede následovně:
  - Pokud je vlevo od něj místo, v jednom tiku se otočí doleva a najede na volné místo.
  - V opačném případě zkoumá co je před ním, když tam je místo, jede dopředu
  - Pokud před ním místo taky není, otočí se doprava.
- V případě kolize má přednost robot, který jede víc "shora" a "zleva".

( Chování robotů je inspirované poměrně starou hrou [Down-to-Earth](https://worldofspectrum.org/infoseekid.cgi?id=0001470) ([video](https://www.youtube.com/watch?v=Uzf4gtaiOcs)). )

## Úkol

Vyrobte jednoduchou simulaci takové jeskyně. Simulátor by měl uživateli dovolit jezdit po mapě nějaké přednastavené velikosti kurzorem, ovládaným například pomocí šipek, a na pozici kurzoru umisťovat stěny nebo roboty, případně tyto objekty mazat (tj. "umisťovat" tam prázdné místo). Uživatel navíc nějakou speciální klávesou (např. mezerníkem) může nechat simulaci proběhnout o 1 časový tik dopředu, tj. roboti by se měli pohnout podle svých pravidel.

Jak nastavíte velikost mapy je jedno, ale simulátor by měl podporovat aspoň 10x10 políček.

## Návod

Použijte nějakou Haskellovou knihovnu na výrobu jednoduchých grafických aplikací, doporučuju jednu z následujících:

- [Gloss](https://hackage.haskell.org/package/gloss-1.13.1.2/docs/Graphics-Gloss.html) -- dostanete jednoduché grafické okno do kterého můžete kreslit vektorovou grafiku, knihovna navíc podporuje podobné jednoduché skoro-hry pomocí funkce [`play`](https://hackage.haskell.org/package/gloss-1.13.1.2/docs/Graphics-Gloss.html#v:play).
- [Brick](https://github.com/jtdaugherty/brick/) -- knihovna účelem podobná Glossu, ale místo grafického okna pro vektorovou grafiku dostanete terminálové okno pro kreslení ASCII grafiky.

Grafickou reprezentaci robotů a ostatních objektů nepřehánějte, v případě Glossu stačí obarvený nasměrovaný trojúhelník, v případě Bricku tradiční ASCII mapa podobná např. nethacku (`.` je prázdno, `#` je stěna, `^` je robot mířící nahoru, typ robota můžete rozlišit barvou, kurzor podbarvením, ...).

Pro oba případy můžete využít jednoduché připravené aplikace, které "už něco dělají":

- [gloss.hs](./gloss.hs) -- potřebuje knihovnu `gloss`, tu si můžete nainstalovat pomocí `cabal install gloss`, zdrojový kód "mimo projekt" skompilujete pomocí `ghc -package gloss gloss.hs -o gloss`
- [brick.hs](./brick.hs) -- potřebuje knihovny `brick` a `vty`, po jejich nainstalování kompilujte pomocí `ghc -package brick -package vty brick.hs -o brick`

### Jak rozumně provést "update mapy"?

Kvůli možným kolizím robotů je bohužel update poněkud těžké naprogramovat jednoduše. Doporučuju následující strategii:

- Definici mapy si rozšiřte o políčko "rezervováno pro robota X"; já jsem použil něco jako `data MapTile = Empty | Wall | Robot ... | ReservedFor ...`
- V prvním kroku updatu roboty ještě nepřesouvejte, ale updaty uložte do mapy jako rezervace. Do rezervace si můžete uložit i jak robot bude vypadat po ukončení kroku (tj. jaký typ robota to bude, a jak má být otočený). Pro ostatní roboty se rezervovaná políčka chovají jako "obsazená". Pokud se robot rozhodne stát na místě (a jen se otočit), přepište ho rezervací, ve které je uložen jeho stav po otočení.
- V druhém kroku updatu všechny políčka s rezervacemi přepište na odpovídající roboty, a roboty kteří zbyli z prvního kroku nahraďte prázdným políčkem.

Vzhledem k tomu, že v prvním kroku musíte reagovat na změny stavu mapy ještě když mapu procházíte, se dost hodí celou věc pojmout jako stavový výpočet, např. jako `updateMap :: State [[MapTile]] ()`. Práci si pak můžete výrazně zjednodušit tím, že si vyrobíte "imperativní" pomůcky jako `getMapTileAt :: Int -> Int -> State [[MapTile]] MapTile` a `setMapTileAt :: Int -> Int -> MapTile -> State [[MapTile] ()`, a algoritmus napíšete stejným stylem jako v imperativních jazycích.

## Bonusy

- Celou mapu můžete reprezentovat jako seznam seznamů, ale mnohem lepší je použít arraye (přecejen jsou rychlejší...). Nízkoúrovňové funkcionální arraye z `Data.Array` jsou poněkud moc generické (tj. pro začátečníky nepoužitelné), proto je výrazně lepší je používat obalené v [`Data.Vector`](https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector.html). Vektor je v tomto případě víceméně ekvivalent konstantní C-čkové arraye.
- Vektory jsou `konstantní' a copy-on-write, tj. změna jednoho prvku někde uprostřed je docela drahá. Místo toho můžete zkusit jednu z následujících možností:
  - Vektor můžete nejdřív vyrobit jako seznam (to je díky lazy vyhodnocování většinou "zadarmo"), a na ten pak zavolat `fromList`.
  - Použít [`Data.Vector.Mutable`](https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector-Mutable.html) (to je víceméně obyčejná C-čková zapisovatelná array). Čtení a úpravy mutovatelného vektoru jsou ale operace, u kterých záleží na pořadí provádění, takže je nutné je "spojovat" opatrně, stejným mechanismem jako IO akce. [Příklad s bubblesortem](https://www.ksi.mff.cuni.cz/~kratochvil/haskell/source/MVectorBubbleSort.hs).

## Odevzdání a hodnocení

Z programu vyrobte cabalový balík pojmenovaný `u1-vaseprijmeni`, ten zabalte pomocí `cabal sdist` a nahrajte do odpovídající kolonky do SISu.

Snažte se o hezký, přehledný, čitelný a _krátký_ kód. Tj.: pokud možno neřešte všechny problémy "ručně" rekurzí, ale používejte rozumné vyšší funkce (`map`, `zip`, `fold`, ...). Opakovaný kód vytrhněte do vlastní funkce a nahraďte nějakou zkratkou (ve funkcionálním programování jde vytrhnout a zkrátit prakticky cokoliv).

Odchylky od specifikace (např. mírně jiné chování robotů nebo jinak vyřešené kolize, jinak vyřešené ovládání, úplně jiný interface, atd.) jsou v rozumné míře OK. Konkrétně, udělat update mapy a pohyb robotů stoprocentně správně je spíš technický problém než něco co by šlo naprogramovat úplně "hezky", takže potíže v updatu hodnotit moc nebudu.
