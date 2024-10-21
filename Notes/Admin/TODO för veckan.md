- Kolla på att effektivisera PW-beräkningar.
	- Verkar som att Christian har lämnat en TODO: memoize i PW-koden på zoomHalf
	- Idé: Om vi utför samma transformation många gånger borde det vara snabbare att räkna ut transformations-matrisen en enda gång och sedan använda den för kommande transformationer. 
		- TODO: Räkna hur ofta samma transformation utförs.
- Separera ThresholdFuns och NormalizedThresholdFuns
- Försök använda Feat för att enumerera funktioner med en viss bitbredd. Det blir nog bäst om vi även enumererar partitioner, snarare än skapar en lista över alla.
- Implementera lifted functions
## Homework

- Se om man kan dra paralleller till entropi (Shannon) eller Huffman coding
- Arvid: Kör Thresh.iteratedMaj 3 3 med profiling-information.