## Earlier

- Made symmetric more efficient by using sequences and ranges instead of a simple list
## 8/10 2024

- Kollade på D(f) == D(! . f). Det gav inte jättestor vinst.
	- Den vinst man får är beroende av att man hittar funktioner som är not av en annan funktion. Och även om vi gör det blir det inte en jättereduktion av problemet.
	- Vi definierade canonical form som den funktion som ger 0 då alla inputs är 0.
- Conversions, mellan symm och gen, samt mellan thresh och gen. Vi skrev en för BasicSymmetric, ganska ineffektiv. Vi skrev även en för Iterated ThresholdFun som baseras på att generera upp alla ITFs och se om en GenFun är ekvivalent med någon av dem.
- QuickCheck

## 9/10 2024

- Funktionalitet för att räkna ut average nodantal i BDDn som motsvarar en funktion.
- Functionality for measuring various statistical properties of the number of nodes in BDDs.
- Separated the normalization step of BoFuns

## 10/10 2024

- Skapade en ny typ NormalizedGenFun som wrappar GenFun. Tanken är att instanser av denna typ alltid ska vara normaliserade. Detta lät oss också skriva en mycket mer effektiv variables, som helt enkelt blir \[1 .. n].
- Har testat att (var 1 .&&. var 2) och (var 2 .&&. var 3) blir equal enligt == efter normalisering. Detta är mycket lovande.
- Lade till en Hashable-instans för Iterated.
- Explicit memoization av BDDs. Detta gav stora förbättringar.
- Vi har gjort massa mätningar, finns i Runtime table
- Vi har kommit fram till att normalisering fortfarande ger en stor (x10) tidsvinst även med den nya memoizationen av BDDs.
- Testade att lägga in support för att memoiza speglad komplexitet vid inverterad input. Detta gav dock ingen större effekt.