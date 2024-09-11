
Vi har nu implementerat alla nödvändiga operationer förutom * och + för algebraiska tal. 

Den approach vi har hittat för att multiplicera algebraiska tal ser ut såhär:

1. För två algebraiska tal a och b har vi två två polynom pa och pb.
2. Dessa polynom är karakteristiska polynom för en matris med polynomets rötter som egenvärden. Vi kan skapa denna matris mha. companion matrix. Detta steg är relativt enkelt.
3. När vi har två matriser A och B för respektive pa och pb kan vi beräkna tensorprodukten av A och B. Denna produkt kommer ha värdet a * b som ett av sina egenvärden. Om vi kan hitta det karaktäristiska polynomet för denna matris är vi alltså klara (förutom att vi måste krympa vårt intervall).
4. Av det vi har sett behöver vi hitta determinanten av (AxB - lambdaI), då denna är lika med det karaktäristiska polynomet. 
	1. En lösning är att vi själva definierar en funktion som räknar ut determinanten mha. de typeclasses som finns i the DSL of Algebra. Det finns dock en risk att operationen blir ineffektiv för stora polynom.
	2. Alternativt hade man kunnat försöka nyttja ett redan existerande matris-bibliotek, men då måste vi hitta ett sätt att kunna använda polynom som värden i dessa. Ett problem är att många kallar på C-kod, och då kan vi inte riktigt ha arbitrary precision.
		1. Eventuellt skulle vi kunna nöja oss med något som funkar upp till 64-bit tal.

https://math.stackexchange.com/questions/2911256/a-way-to-represent-algebraic-numbers-in-a-computer



