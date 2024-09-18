
Antal subfunktioner:

Vi börjar med en funktion med n oevaluerade bitar. Genom att specificera en av dem får vi en ny subfunktion med n-1 oevaluerade bitar.
Vi väljer bland n bitar och varje bit kan ha ett av två värden, vilket ger 2*n möjliga subfunktioner från första nivån.
Var och en av dessa har i sin tur 2*(n-1) subfunktioner, osv. vilket ger oss totalt 2^n * n! subfunktioner.
Flera av dessa kan dock vara identiska, det är här memoization kommer in i bilden. Om vi har räknat ut komplexiteten för en viss funktion
behöver vi aldrig göra det igen, vi kan bara återanvända resultatet.

Antal evalueringsordningar: