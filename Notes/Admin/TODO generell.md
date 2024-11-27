
## Kod

- Hög prio
	- Sätt upp de mät-strukturer vi behöver
	- Sista cleanup innan vi lämnar in
- Låg prio, kan skippas
	- Pretty printing av funktioner
## Mätningar

- Bestäm vad vi vill mäta och presentera i resultatdelen, även hur vi vill presentera det.
- Mät egenskaper hos de klasser vi har undersökt
- Utforska iterated majority mer, just nu har vi mycket flat maj
- Eventuellt: Kolla på funktioner med konstant komplexitet
	- För dessa finns en definitivt snabbast evalueringsordning
## Svåra grejer

 - Räkning av funktioner
	- Vi har provat approachen att generera alla representationer och sedan konvertera till BDD för att avgöra antalet unika funktioner. Denna approach hade ett antal problem, för det första har vissa klasser ett oändligt antal representationer. För det andra har vissa klasser väldigt många representationer i förhållande till antalet unika funktioner i klassen.
	- Vi behöver komma på en ny approach
		- En idé kan vara rewrite rules för att få en unik representation per unik funktion.
	- Alternativt kan vi bara räkna de som är lätta att räkna, eller där det finns data sedan tidigare. Man kanske kan räkna ut ett bound för de svåra?
	- Vi kan verifiera våra resultat med the dictionary of integer sequences.
	- Viktigt att komma ihåg vad syftet med detta är, vi vill på något sätt mäta hur mycket impact en speedup för en viss klass skulle ha. Men vi har t.ex. få NGFs, men varje GF kan konverteras till en NGF med samma eller lägre antal bitar och med samma komplexitet.
	- Ny idé om generering: Följer egentligen approach 1. Rewrite rules:
		- Om vi har en lifted function som är konstant, kan denna bytas ut mot motsvarande 0-bitarsfunktion som är konstant, och därmed inte lyft över något.
		- Om vi har lyft id-funktionen, är detta ekvivalent med den underliggande funktionen, och man kan alltså kapa ett lager.
## Future work

- Effektivare polynomberäkningar
	- Idé: Om vi utför samma transformation många gånger borde det vara snabbare att räkna ut transformations-matrisen en enda gång och sedan använda den för kommande transformationer. 
		- Man bör i så fall först räkna hur ofta samma transformation utförs.
- Fler subclasses implementerade
- Använd rewrite rules för normalisering av exempelvis lifted functions.
- Conversions till/från olika klasser
	- Just nu har vi enkla conversions
		- GenFun to SymmetricFun - kollar om en funktion är symmetrisk och konverterar till SymmetricFun-implementation.
		- NGF to Iterated ThresholdFun - genererar alla n-bits ITFs och kollar sedan vilka som representerar samma funktion som input-NGFen.
		- boFunToGenFun konverterar från BoFun till GenFun. Denna riktning är generellt sett enkel.
	- Conversions från GenFun till övrigt är i regel svårt. Patrik har föreslagit att vi baserar översättningen på rewrite rules eller i övrigt utgår från struktur.