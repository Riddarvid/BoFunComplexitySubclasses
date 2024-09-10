Tänk att du har en boolsk funktion som tar n bitar.

Vi kan nu undersöka funktionens bitar i olika ordning för att komma fram till ett resultat.
Den här ordningen kan bero av vilka värden bitarna vi undersökt hittills har haft. 
Vid någon punkt i evalueringen har vi ett resultat, som kan nås olika snabbt beroende på bitarnas värden.

Om vi antar att bitarnas värden är oberoende och följer en bernoullidistribution kan vi räkna ut det väntade antalet bitar som kommer behöva evalueras
för en given evalueringsordning (binary decision tree).

Om vi hittar detta väntevärde för alla möjliga evalueringsordningar kan vi välja den som har lägst väntevärde.
Detta definierar vi som level-p-komplexiteten för funktionen.




"Given a Boolean function f, a decision tree t describes one way to evaluate the function f."