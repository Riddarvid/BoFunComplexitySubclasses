
- Vad vi har gjort:
	- Testat att lägga till support för D(f) == D(! . f)
- Hur görs memoizationsteget? Varför måste det ligga utanför hela beräkningen?
- Vår förståelse av memoize är att man vill ta in ett värde, dekonstruera det till ett värde som är Memoizable, sedan vill man applicera en funktion som bygger upp originalvärdet och sedan applicerar funktionen f. Denna funktion blir då Memoizable pga. att inputvärdet är det. Är detta korrekt? Det blir i så fall begränsande i de fall där vi kan dekonstruera exempelvis en BDD till en unik int, men inte kan gå åt andra hållet.
- Kan man få access till kraftigare datorer för att utföra mer krävande beräkningar?
- Memoization av polynomberäkningar?
- Hash consing for fast equals test? Hur funkar det?