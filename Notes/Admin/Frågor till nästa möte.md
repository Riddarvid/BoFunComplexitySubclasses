Frågor: 

- 2.1.4 Vår def av hash-consig är korrekt (är unique integer) men burde vi förklara det mer?
- 2.4 Hur kan vi förbättra våra bevis?
	- Var väldigt entydig, "förklara för datorn"
	- Definiera saker ordentligt.
- 6.2: "How are function built? Do you need to know special syntax/rep for a class or can you use general combinators?" - Patrik 
	- Kanske snarare i resultatdelen, förtydliga att de olika kolumnerna i tabellen utgörs av olika datatyper. Förtydliga även hur användaren ska använda biblioteket i detta avseendet, tänk användarvänlighet, eventuellt smarta konstruktorer.
	- Ex. för lifted och iterated exponerar vi trevliga konstruktorer, man behöver ej kunna de interna konstruktorerna.
- Sources
	- Sätt klamrar runt stor bokstav i egennamn
	- Bibliography: including the version of the code from JanssonJansson2023?
		- Länka endast till repot, lägg början av commit hash i notes. Alternativt använd typ href för fin länk.
- Borde vi byta ut Free f () mot Maybe (f (Iterated f))
	- Nej, det går inte
	- Det kan vara så att vi faktiskt har nytta av monadinstansen av Free (). Ex. att kunna ersätta varje variabel med en ny itererad funktion.
	- Överväg om/hur vi vill göra om Free-typen.
- Hur kan vi identifiera maxima/minima inuti ett polynom-segment?

Använd lhs-to-tex för att generera snygga listings.

https://github.com/patrikja/skeleton

