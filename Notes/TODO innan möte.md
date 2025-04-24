### 2.3.1

* The induction step in the proof of Lemma 2.3.4 has the same problem as for Lemma 2.3.1.
  You rely on J₀ = J - i.

* I think there is a problem in the proof of Theorem 2.3.6.
  In the definition of complexity, we first go over all decision trees for the function and for each of them calculate the average cost over all inputs.
  But in this proof, you first fix an input vector and then look at the decision tree minimizing the cost for that input vector.
  In particular, you may get different decision trees for different input vectors!

## Mina takes

2.3.1 borde nog ändå gå ganska lätt att fixa, känns som att hinten var the missing piece of the puzzle. Jag har ju själv insett att problemet är att jag i ena scenariot får J-i, men inte sett hur jag ska kunna applicera induktionshypotesen eftersom den kräver J_0. Då är det ju på ett sätt väldigt logiskt att lösningen är att ändra induktionshypotesen, som alltså ställt ett för strikt krav. Känns ändå görbart!

2.3.2 - verkar bara vara några små saker att ändra, men se till att ändå ha koll på hela processen.

2.3.4 har tydligen samma problem som 2.3.1, men borde ju då kunna fixas på samma sätt.



## Plan

1. Fixa 2.3.2.
2. Fixa 2.3.4.
3. Fixa det slutgiltiga teoremet, 2.3.6. I samband med detta, eller kanske helst innan, förstå vari problemet ligger och jämför med 2.3.10.