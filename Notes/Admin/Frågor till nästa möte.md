
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



Frågor: 

2.1.4 Vår def av hash-consig är korrekt (är unique integer) men burde vi förklara det mer?

2.4 Hur kan vi förbättra våra bevis?





Inför release:
- CI mha github actions för att bygga och köra tester vid merge.
	- Finns ett paket (haskell-ci) som genererar github action baserat på koden.

Se till att lämna in innan onsdag kväll
