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
- Implementerade enumerable för Symmetric (och får gratis instans för ITF).
- Refaktorerade Symmetric

## 21/10 möte

- Vad vi har gjort:
	- Refaktorerat Symmetric
	- Implementerat Enumerable för Threshold, Symmetric, och Iterated.
	- Försökt skriva en effektivare composition specifikt för affina transformationer mha. Taylor-expansion. Resultatet blev inte bättre än det vi redan har.
	- Arbetat med rapporten
- Borde vi separera ThresholdFuns och NormalizedThresholdFuns? I så fall skulle vi kunna definiera särskilda invarianter specifikt för NTFs.
	- Ja
- Hur ska vi göra med 0-ary subfunctions? De är okej när vi enumererar, men inte när vi vill generera alla n-bits funktioner.
	- Ta endast med dessa när vi specifikt är intresserade av bitbredd 0 på toppnivå, annars inte.
	- Kan vara intressant att jämföra de första x elementen när man parameteriserar över bitbredd vs. ej
	- Tanke: Det skulle kunna finnas någon form av samband mellan komplexitet uttryckt i antal konstruktorer och i level-p. Ta stickprov och evaluera i p = 1/2 tex.
- Rörande effektivisering av PW-beräkningar: 
	- https://stackoverflow.com/questions/141422/how-can-a-transform-a-polynomial-to-another-coordinate-system
	- Om vi utför samma transformation flera gånger borde detta vara någorlunda effektivt.
- Rörande rapporten: Hur borde vi ta upp saker som inte fungerade? Saker där vi testade men ännu inte vet om de verkligen inte funkar?
	- Definitivt positiva/negativa saker ska vara med. Om man bara inte lyckades komma fram till något behöver man inte nämna det mer än i förbifarten (om ens det). I masterarbeten särskilt är definitivt negativa resultat intressanta.
	- Struktur:
		- 1) Pedagogisk: Hur kom vi fram till det vi kom fram till. Inte särskilt många sidospår osv.
			- En sektion där man presenterar det positiva, sedan en sektion med det som inte fungerade.
		- 2) Appendix: Här kan vi lägga det som inte fungerade. 
			- Låter som en god idé

### Sammanfattning:

- Vi borde separera ThresholdFuns och NormalizedThresholdFuns
- Vi borde kunna skriva en enumereringsfunktion för specifika bitbreddar. pay behöver inte motsvara bit-kostnaden.
- Patrik tycker att en matris-representation av en transformation är rimlig.
- Det lät bra att lägga saker som misslyckades i ett appendix.

Exempelkod för enumeration:

type Bag n k a -- innehåller exakt k element av typ a, med total "bitbredd" n

xs :: Bag 7 5 "BF"
xs = [(2,And2),(3,Id1)]
bitbredd xs = 2*2+3*1 = 7

Feat a ~= Set a
  -- dessutom enumererad, dvs. uppdelad i massor av "småmängder" av bestämd storlek

önskas :: (n : Int) -> Feat (BF n)

help :: ((n : Int) -> Feat a) -> ((n : Int) -> Feat (Bag n ? a))
help fas = fbs
  where fbs n = let pss = partitions n  -- Kanske partitions ska ge en Feat [Int]
                    xs = ... map fas ps ...
		in {- combine them to a bag -}
		
  -- as :: Feat a, 

## 29/10

- Vi har beslutat oss för att pausa fokuset på FEAT. Vi har redan funktionalitet för generering av samtlig funktioner, men som använder listor istället. Vi stöter på problem i FEAT eftersom vi får typ Feat (Feat f), men eftersom Feat inte är en monad kan vi inte använda join för att få en Feat a.
- Vi känner oss relativt klara med Lifted och Iterated för tillfället.

## Möte 4/11

- Vad vi har gjort:
	- Arbetat med enumeration igen men kommit fram att vi lägger för mycket tid på det gentemot vad det ger. Vi har istället fokuserat på funktioner som genererar samtliga funktioner utan att använda Feat.
	- Skrivit rapport
- Rörande rapporten
	- Hur ska vi göra när vi vill ge credit till Sattler och Hughes
		- Vi har nog endast använt det Christian har skrivit. Vi lägger till repot som källa, misc.
		- I contribution vill vi lista vilka funktionsklasser vi nu supportar. Kan vi nämna Threshold functions här, och hur borde vi ta upp det?
	- Just nu ligger "What can be improved?" i teori-delen. Vi presenterar dock nya grejer som vi själva upptäckt. Hade det varit mer passande som en kort metod-del?
		- Borde inte vara teori
	- Vad tycker du att vår meta-sektion borde heta? Dvs. den som just nu heter "Examining Complexities and Verifying Results".
	- Vi vill hävda att komplexiteten inte förändras när man ändrar variabel ordning, så t.ex. x1^x2 har samma komplexitet x2^x3. Antar att vi behöver skriva ett bevis för det. Borde det ligga i teoridelen eller någon annanstans?
		- Indelning i teori/annat är inte superviktig, det viktigaste är att presentera ämnet på ett sådant sätt att läsaren förstår det.
		- https://simon.peytonjones.org/great-research-paper/
		- Egenskaper vi borde bevisa:
			- En funktions komplexitet är endast beroende av dependent bits.
				- semantisk egenskap
			- Att substituera andra variabelnummer kommer inte påverka en funktions komplexitet.
				- syntaktisk egenskap
				- Kan generaliseras till "alla syntaktiska funktioner som har samma semantik har samma komplexitet"
	- Är det okej att ge en inkorrekt definition i rapporten, som en förenkling av den faktiska koden?
		- Ja typ, inte inkorrekt men förenklad
- Hur borde vi lägga upp arbetet nu? Fokus på rapport kontra annat.
- I vilken grad bedöms koden?
	- Fråga Christian


Inför release:
- CI mha github actions för att bygga och köra tester vid merge.
	- Finns ett paket (haskell-ci) som genererar github action baserat på koden.

Se till att lämna in innan onsdag kväll