- Förtydliga properties
- Använd hls2Tex för code listings
- Lägg in komplexiteten av IterMaj 3 3 i appendix A
- Se om vi vill lägga till "När vi skriver effektiv eller optimerad menar vi alltid med avseende på komplexitetsberäkningar om inget annat sägs". I introduktionen någonstans?
- Förklara de nya box-plotsen när det är dags
- Ta med att vi i nuläget har flera representationer av samma funktion för en given datatyp (ex. Iterated ThresholdFun)
	- Räcker nog med ett kort exempel, typ 3-bit or = 2-bit or + id.
- Skriv om de egenskaper vi har hittat hos komplexiteter (mirroring osv.)
- I samband med våra grafer borde vi ha en hypotes om det de visar. Ex. om vi ser ett linjärt beteende, vad är k och m? Vad betyder det?
	- t.ex. "Kvoten mellan körtiderna ökar exponentiellt"
- Uppdatera rapporten med korrekta properties.
- Skriv klart bevisen
	- Kolla Julias källor och se om det finns bevis där
- Se över om vi vill lägga till saker som inte fungerade i appendix, t.ex. mirroring.
- Critical points
	- Döp om tools
	- 5.1 Förklara hur signAtAlgebraic funkar, gärna med illustration/diagram.
		- I allmänhet är det nog konceptet shrinking som behöver förklaras
	- Se över all text som rör detta, även code listings.

## Latex-grejer

- Se till att använda macros överallt, för \T, \F, och \setBit
- Kör en ctrl+f och ersätt subfunction med sub-function
- Fixa biblography (upper case, too long links)
- Referera till bilden på framsidan 
- Alla kommentarer

## Idéer

- Använd short-circuiting av boolean expressions som mer relaterbart exempel

## From writing seminar: 

Peer Review: 

Too many subsections with short paragraphs. Lack of transition 
- expand on the sections 
- remove a lot of introduction sentences

explain the terms more in the introductions. like in the contribution. expand on the terms. and perhaps add another example (or the maj example) in the introduction.  

need more citations and sources 

Flamegraphs: mostly good, but sometimes the specific functions are hard to see in the graphs. Add also some axis on the diagram

Potential profiling tool: valgrind, used for c but possibly work for Haskell

The representations section may need to be renamed. Perhaps it is better to be Implementations and results

tables should have descriptions below. 

make better listings (so they stand out more)
- Minted



Language: 

use "we" a lot. Especially in the theory. 

section titles - should we capitalize all the words or not? ask Patrik 

we use quotation when we should use bold

add more reflections in the discussion. Right now it is a lot of summary. Put that summary first and then reflections


