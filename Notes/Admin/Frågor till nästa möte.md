
- Vad vi har gjort:
- Frågor om Feat:
	- Hur ska vi tänka när vi använder Feat? instinktivt känns det som att vi vill använda det som list-monaden eller Gen-monaden från QuickCheck (förutom att det inte är en monad).
	- Hur ska vi hantera kostnad? Är det okej att göra som vi har gjort nu, dvs bara calla pay i en "loop". Detta känns fel.
- Borde vi separera ThresholdFuns och NormalizedThresholdFuns? I så fall skulle vi kunna definiera särskilda invarianter specifikt för NTFs.