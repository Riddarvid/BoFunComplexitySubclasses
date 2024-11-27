- Hur tung är root counting kontra polynomevaluering? Vi vill använda detta i våra Algebraic numbers.
	- Beräknas från teckenskiftningar i koefficientlistan - samma som evaluering.
	- Mät skillnaden i effektivitet!
- När vi letar extrempunkter för en PW, hur borde vi hantera x=0 och x=1?
	- I första hand, kolla derivatan direkt - om den är skild från 0 är vi klara. Annars, kör shrinking tills vi har ett interval med 0 rötter, kolla någon punkt i det intervallet.
- Hur ska vi göra om pieces bildar ett "tak" där taket har lutning 0? Är det ett maxima?
- Om det skulle vara så att vi inte får till våra bevis, kan man hänvisa till testning istället?
	- Skriv conjecture istället, hänvisa till textning.
- Vi har haft svårt med att räkna antalet itererade funktioner. Är det okej att stryka det som sektion i koden och istället nämna kort att det inte är så stor andel av funktioner som är symmetriska eller threshold functions?
	- Skulle kunna lägga in conjecture här också, övre och lägre bounds.
- Hur ska vi göra med de egenskaper vi har hittat men som vi bestämt oss för att inte använda, typ invertering av inputs ger mirroring av komplexitet?
	- Kan dela in egenskaper först i samtliga vi har undersökt, sedan de vi faktiskt har använt / gav tidsvinst.
- Har vi tillräckligt med källor?
	- Lägg ett antal timmar på litteratursökning under rapportskrivningen. Related work är det som driver source count. Sök på nyckelord osv.
	- Matematiska Wikipedia brukar ha bra källor

I allmänhet om extrempunkter:
- Hantera derivata 0
- Tänk i termer av att först identifiera kandidater och sedan bestämma deras karaktär.



Vi behöver fixa en top-level README som förklarar struktur, hur man använder koden.