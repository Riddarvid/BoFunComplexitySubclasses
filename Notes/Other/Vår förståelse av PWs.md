
## ZoomData

En Zoomdata specificerar ett visst intervall som vi zoomat in på. Den gör detta via en zoomlevel och en affin transformation (translation + skalering).

Semigroup-instansen för ZoomData innebär att man kombinerar zoomleveln (om det tidigare var x4 och x8 får man nu x32) och vi kombinerar de affina transformationerna med function composition.

mempty-transformationen är p(x)=x

zoomEval evaluerar den affina transformationen i en zoomdata i en viss punkt.

## Zoomable

En zoomable har en funktion zoom, som givet en ZoomData ger en funktion från x till x. Denna tar troligen en punkt i något intervall och skalerar upp den till (0, 1) eller tvärtom.

## Nyckeln

Nyckeln till det hela verkar vara instansen Zoomable a (Poly a).

Dvs. en ZoomData ger oss ett sätt att "zooma in" på ett polynom. Om du har ett polynom och du är intresserad av intervallet (a,b), men har en funktion som ger dig resultat för intervallet (0,1) behöver du ett sätt att translatera och skalera polynomet så att det har samma relevanta egenskaper i (0,1) som det förut hade i (a,b). Det enda som behövs för detta är en skalär och en offset, och detta är exakt vad typen AffinePoly och därmed ZoomData fångar. Vill man zooma flera gånger kan man helt enkelt applicera flera av dessa transformationer i rad.

## Zoomed

En zoomed är någon data, datan zoomad, samt zoomen som användes för att zooma.

zoomedTrivial: Zoomed with no zoom

zoomedGenerate: Givet data och zoom, zooma datan

zoomedRegenerate: Givet Zoomed och ny zoomdata, omzooma originaldatan

## ZoomedApprox

Väldigt oklart just nu

Används för diverse approximationer tror vi.

## Other

biscted ger oss x inzoomad på det lägre respektive det högre intervallet (antaget att originalintervallet var (0,1))

## Intersect

En intersect är helt enkelt två polynom och ett algebraiskt tal som talar om var de skär varandra.