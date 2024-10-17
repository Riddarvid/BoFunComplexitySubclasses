## Earlier

- Made symmetric more efficient by using sequences and ranges instead of a simple list
- Implemented iterated gates
- Algebraic numbers
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

## 11/10

- Cleanup
- Förståelse av PiecewisePoly

## 14/10 möteslog

- Vad vi har gjort:
	- Skrivit en ny computeMin som nyttjar fast comparison av BDDs. Vi vet ännu inte exakt hur denna funkar, men har mailat maintainern av repot.
	- Mätningar av de olika algoritmerna
	- Testat att utnyttja egenskaperna från inverterad output/inputs
	- Konverteringar mellan olika klasser och GenFun
	- Kollat på average nodantal i BDDs. För symmetriska funktioner verkar det ganska linjärt, men vi har inte kollat tillräckligt noga än
	- Bröt ut sakerna som har med normalisering av GenFuns till en egen typ för bättre separation of concerns.
- Hur görs memoizationsteget? Varför måste det ligga utanför hela beräkningen?
	- Specifikt, kan du förklara koden i computeMin?
- Vår förståelse av memoize är att man vill ta in ett värde, dekonstruera det till ett värde som är Memoizable, sedan vill man applicera en funktion som bygger upp originalvärdet och sedan applicerar funktionen f. Denna funktion blir då Memoizable pga. att inputvärdet är det. Är detta korrekt? Det blir i så fall begränsande i de fall där vi kan dekonstruera exempelvis en BDD till en unik int, men inte kan gå åt andra hållet.
- Kan man få access till kraftigare datorer för att utföra mer krävande beräkningar?
- Memoization av polynomberäkningar?
- Hash consing for fast equals test? Hur funkar det?

## 14/10

- Möte
- Undersökt Feat-biblioteket

## 15/10

- Fortsatt att undersöka Feat
	- Vårt första försök blev inte jättebra. Det är troligen för att vi säger att man måste generera alla 6-bitars funktioner för att kunna generera en enda 6-bitars funktion.
- Separerat generering av alla ITFs från generering av enskilda ITFs.
- Teori:
	- Just nu går mycket tid i Polynomberäkningarna till funktionen comP, som används när vi vill zooma ett polynom. comP är generell, så vi tänker att vi skulle kunna vinna något genom att skriva en specialiserad version för just affina transformationer.
	- Verkar som att vi kan använda Taylor expansion för shift, skalering är enkelt.
- Just nu sitter vi fast på offset-beräkningen. Vi är inte riktigt med på hur Taylor expansion funkar eller om det ens skulle vara snabbare än det vi gör nu med comP.

## 16/10

- Vi har undersökt Taylor expansion och kommit fram till att vi kan använda den för translation. Vi har testat att en affin transformation kan utföras genom att först utföra en translation och sedan en skalering. Planen nu är att använda Taylor expansion för translationen, sedan är skalering en enkel linjär operation.
- Från vad vi kan se är Taylor-metoden inte snabbare än den tidigare implementationen. Den verkar dock inte vara långsammare heller, vilket är trevligt.
## 17/10

- Vi har nu skrivit en "bättre" implementation av Enumerable för ITFs. Den är inte kopplad till bitantal, men den är garanterad att endast generera giltiga funktioner. Skillnaden nu är att vi faktiskt jobbar med operationer på applicative functors istället för att generera en gigantisk lista och sen konvertera den till en Sharable.
- aconcat av en lång lista blir ineffektivt eftersom vi måste iterera igenom hela listan för att få reda på kardinaliteten.
- Vi har nu brutit ut iterated-delen av enumeration till Iterated-filen. Inget i denna fil kostar, vilket gör att kostnaden helt bestäms av den underliggande funktionen.
- Nackdelen med Feat-enumeration är att vi inte har kontroll över funktionens arity. Vi borde alltså fortfarande använda QuickCheck när det är önskvärt. Vi kan dock använda feat när det inte är viktigt.
- Vi får fundera på om Feat-enumeration borde tillåta 0-ary sub functions, eller om vi ska ha som invariant att ThresholdFuns aldrig får ha 0-ary sub functions.
- Potentiell fråga: Borde invarianten vara ännu starkare: dvs. att vi inte ens tillåter konstanta subfunktioner? Detta är vad som händer i setBit för threshold functions just nu.