- Lägg in den förenklade Iterated-definitionen i en separat fil och se till att den kompilerar.
- Skriv till Christian och fråga hur bedömningen av arbetet ser ut, fokus på kod kontra rapport.
- Problemet med vår approach av att gnerera alla representationer och sedan konvertera dem till BDDs är att vissa funktioner har oändligt många representationer (specifikt har vi stött på detta problem i Iterated).
	- Approach 1) Ändra datatypen för iterated/lifted
	- Approach 2) Ändra genererings-algoritmen

Ny idé om generering: Följer egentligen approach 1. Rewrite rules:
- Om vi har en lifted function som är konstant, kan denna bytas ut mot motsvarande 0-bitarsfunktion som är konstant, och därmed inte lyft över något.
- Om vi har lyft id-funktionen, är detta ekvivalent med den underliggande funktionen, och man kan alltså kapa ett lager.
## Homework

- Se om man kan dra paralleller till entropi (Shannon) eller Huffman coding
- 	Selina har kollat på det. har mycket gemensamt men ser inte just nu hur vi kan använda det. 
- Arvid: Kör Thresh.iteratedMaj 3 3 med profiling-information.
- https://simon.peytonjones.org/great-research-paper/
