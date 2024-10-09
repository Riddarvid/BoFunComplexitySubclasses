Om vi har en BDD kan vi enkelt få fram hash-värdet (O(1)). Vi vet att om vi har olika hashvärden kan BDDsen omöjlig vara lika. Vi skulle därför vilja göra denna jämförelse först, och sedan använda oss av vanlig memoization om det visar sig att de har samma hash-värde.

Detta bygger på antagandet att hash-implementationen för BDDs alltid ger samma hash-värde för identiska BDDs.