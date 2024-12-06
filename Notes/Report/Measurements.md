
## Comparing a specific function

Bar chart for comparing the different algorithms and representations for the function maj_11

Skulle kunna köra maj 2 3 också?

Våra optimeringar verkar funka bra för den här funktionen

## Comparing maj_n for different values of n

Just nu har vi tre diagram, ett för varje algoritm.

- TODO: Expandera de nuvarande diagrammen till höger för de representationer som klarar det. Sätt max-värde 10 på tiden.

En slutsats man verkar kunna dra från dessa diagram är att computeMin inte direkt blir mer effektiv än genAlg, i alla fall för maj_n.

En annan slutsats är att computeMin' verkar köra en faktor 10 snabbare än computeMin, dvs fast BDD comparison har hjälpt.

## Comparing threshold for same number of bits but with differently skewed thresholds

Hypotes: Det tar längre tid att beräkna komplexiteten för en thresholdfun om thresholdet är långt från att uppnås.

## Box plots av random funs

Vi kommer köra 100 genererade funktioner, men bara mäta varje funktion en gång.

En slutsats man kan dra är att vi ser väldigt stor varians för itererade funktioner. Detta beror troligen på att en väldigt stor itererad struktur ändå kan representera en funktion med få bitar. Potentiell future work att använda typ reduction rules för att hålla ned storleken på trädet.

I den "snabba" box ploten, börja med Thresh 10, 15, 150, sedan Symm 10, 15, 150, sedan Iter Thresh 10, 15, sedan Iter Symm 10

Vi kan också jämföra de snabba representationerna med general functions för att visa att specialiserade subclasses faktiskt gav resultat.