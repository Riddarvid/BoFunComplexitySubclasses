Min förståelse av memoize-biblioteket just nu

Allt bygger i grunden på typeclassen Memoizable. För att en typ a ska vara memoizable måste den implementera en funktion

`memoize :: (a -> v) -> a -> v`

dvs. givet en funktion från a till v ger memoize tillbaks en funktion från a till v. Mitt antagande är att denna funktion internt utför en lookup.

Vi tittar nu på hur memoize är definierad över Bounded, Enumerable (ändliga) typer.

1) Vi bygger upp ett binärt sökträd där rotnoden blir värdet mitt emellan minBound och maxBound. Varje nivå delar alltid upp det resterande sökrummet i lika stora intervall.
2) Givet att vi vill memoiza funktionen f så använder vi fmap för att mappa varje värde i vårt sökträd till motsvarande resultatvärde.
3) När vi vill kolla upp ett värde gör vi en binärsökning tills vi hittar vårt värde.

Vi kan nu definiera en funktion som till exempel fib på följande sätt:

`fib :: Int -> Integer`
`fib 0 = 0`
`fib 1 = 1`
`fib n = fibMemo (n - 1) + fibMemo (n - 2)`

`fibMemo :: Int -> Integer`
`fibMemo = memoize fib`

Alternativt direkt definierar:

`fib :: Int -> Integer`
`fib 0 = 0`
`fib 1 = 1`
`fib n = (memoize fib) (n - 1) + (memoize fib) (n - 2)`

Alternativt

`fib :: Int -> Integer`
`fib = memoFix $ \fib' n -> case n of`
	`0 -> 0`
	`1 -> 1`
	`n' -> fib' (n - 1) + fib' (n - 2)`

Logiken blir väl att första gången vi kallar memoize så byggs sökträdet "theFinites" upp. Detta är ett top-level name och kommer därför enligt antagandet om GHCs evalueringsmetod evalueras en gång och sedan aldrig mer. Jag gissar att detta gäller så länge vi är kvar i "scopet" av det första callet till memoize. Jag känner att det fortfarande finns några saker som är oklara när det kommer just till vilka saker som evalueras och inte när det kommer till Haskell. Men detta borde vara grundprincipen.

Att det i slutändan blir snabbar med memoization bygger såklart på antagandet att det går snabbare att göra en binärsökning av funktionens domän än att faktiskt evaluera funktionen. Genom att generera sökträdet och sedan köra fmap f på det ser vi till att f evalueras för ett givet värde max en gång, men pga. laziness kommer det inte evalueras om det inte behövs.

------------ Non-finite types --------------------------------------

Okej, detta förklarar hur processen går till för ändliga mängder/typer. Men hur funkar det för oändliga eller rekursiva typer? För det tror jag att vi måste titta på deriveMemoizable och den ser rätt komplicerad ut :/

Det finns även en speciell implementation för Integer och för funktioner. Verkar som att den gör Integer till en lista av Ints. Vi kommer alltså använda Memoizable-instanserna från \[a] och Int för att memoiza Integers.

Den generella metoden verkar vara: 

`memoize :: (a -> v) -> a -> v`
`memoize f = memoize (f . construct) . deconstruct`
	`where`
		`deconstruct :: a -> b`
		`construct :: b -> a`

där b är Memoizable.

----------------------- deriveMemoize ------------------------

Derive memoize:

Taget från dokumentationen:

-- | Build the 'memoize' method. The form of 'memoize' is always

-- @
`--      memoize f = lookup where`
`--        cache1 = memoize $ \x1 -> ... memoize $ \x(a1) -> f (C1 x1 ...)`
`--        ...`
`--        cacheN = memoize $ \x1 -> ... memoize $ \x(aN) -> f (CN x1 ...)`
`--        lookup (C1 x1 ...) = cache1 x1 ...`
`--        ...`
`--        lookup (CN xN ...) = cacheN xN ...`
-- @

-- where @C1@ ... @CN@ are the constructors of the data type and
-- @aj@ is the arity of constructor @Cj@.

Det verkar alltså som att vi bygger upp en cache per konstruktor, där en cache består av att vi bygger en serie av memoize-calls, ett per argument till konstruktorn i fråga. Lookupen blir alltså helt enkelt att dekonstruera värdet och kalla memoize på var och en av parametrarna.

Parametrarna i fråga måste givietvis också vara Memoizable. Detta betyder att vi någon gång kommer komma ned till memoizeFinite eller memoizeInteger eller liknande, i alla fall om man använder samma struktur som här hela vägen ned.