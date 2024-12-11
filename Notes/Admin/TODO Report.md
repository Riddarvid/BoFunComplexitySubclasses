
### High 

- Lägg in en list of definitions - Arvid - tydligen svårt, kolla med Patrik
- Förtydliga definitioner, se feedback: - Arvid
	- Definition 1 needs to be clarified instead of offloading the precise definition to later running text. For example, it does not say if the g_i all share the same input bits or have disjoint input bits (as you want). I suggest that you are precise and annotate not just f, but also the g_i and the "lifting" h with their arity.
	- Similarly, Definition 2 needs to be clarified. What does it mean precisely to repeatedly lift functions of the same type? Is every node an n-ary boolean function for the same n or can the n differ? If you feel that you cannot do this in this section, you can move the definition to where it makes sense. You can still informally describe the notion here, with a reference to the precise definition later on. But I think it is better to give a mathematical definition involving functions here and then later the development involving *representations* of those functions.
- Döp om Lifting till multi composition
	- I would call it (multi)composition. Mathematically, this is composition in the [multicategory](https://en.wikipedia.org/wiki/Multicategory#Examples) of sets and n-ary functions.
- Förklara arities mha. sets istället. Detta borde hjälpa vid förklaring av multi composition.
	- Se feedback i half-time.txt
- ~~Ta upp reduktion av BDDs och hur det bidrar till uniqueness.~~
- ~~Förklara BoFun-typeclassen i större utsträckning.~~
	- ~~The typeclass BoFun should be explained:~~
	  * ~~f is a type representing boolean functions,~~
	  * ~~i is the set of possible variable names.~~
	  * What do the operations of the type class correspond to in terms of the represented mathematical boolean function?
- Bevis - Gemensamt på onsdag
	- Remember to introduce all the notation used in Section 2.4.
	- Definiera independent istället för dependent.
	- Grammatik: "node's": of the node
	- Decision tree istället för valid decision tree.
	- I lemma 2, antag godtyckligt p så blir c1 och c2 tal istället för funktioner.
	- Uttryck "Level-p complexity is dependent only on a function's dependent bits" som en entydig matematisk utsaga.
	- Uttryck "The complexity of a function expressed as a BDD is not dependent on variable numbering.". Se feedback för förslag.
- Skriv om 4.4 för att bättre matcha nuvarande kod.
- ~~Bättre definition av threshold functions:~~ - Selina
	- ~~A more symmetric intuition for this representation is as follows. Suppose k_t bits are true and k_f bits are false. Then k_t + k_f = n, so (k_t - n_t) + (k_f - n_f) = -1. Because these are integers, exactly one of k_t - n_t and k_f - n_f must be non-negative. That is, exactly one of k_t ≥ n_t and k_f ≥ n_f is true.~~
- Se till att uttrycka effiktivitetsförbättringar som improvement factor, snarare än andel tidsminskning.
- Need more citations and sources
- Too many subsections with short paragraphs. Lack of transition - Gemensamt idag
	- expand on the sections 
	- remove a lot of introduction sentences
- Explain the terms more in the introductions. like in the contribution. expand on the terms. ~~and perhaps add another example (or the maj example) in the introduction. - Gemensamt idag~~
- ~~Tables should have descriptions below.~~
- Se över hur vi använder "we" och se till att det inte blir otydligt.
- ~~Ha med exempel i introduktionen.~~
	- ~~Short circuiting av boolska uttryck?~~
- Fixa bibliography (upper case, too long links)
- Fixa bilden på framsidan, så att vi har en beskrivning av den
- Se över alla kommentarer i dokumentet.
- Lägg in data i boxplots, förklara och referera till dem i texten. - Arvid
- Se över properties och se att de är representativa för koden. Se även namn.
- Skriv om Applications för att bättre matcha namn osv. - Selina
	-  5.1 Förklara hur signAtAlgebraic funkar, gärna med illustration/diagram.
		- I allmänhet är det nog konceptet shrinking som behöver förklaras
- Se över alla ställen där vi använder math mode och se om det är mer logiskt att använda typewriter font istället.
### Low

- Om vi har tid över kan vi försöka skapa tydligare flamegraphs. minwidth option?
- Se över hur vi använder citationstecken
- Skriv klart diskussionen. Mindre summary, mer reflektioner.
- Lägg in komplexiteten av IterMaj 3 3 i appendix A
- Ta upp i iterated:
	- Här finns flera representationer av samma funktion.
- I allmänhet, gör hypoteser eller dra slutsatser om det diagrammen visar.
- Se över om vi vill lägga till saker som inte fungerade i appendix, t.ex. mirroring. Optional.
