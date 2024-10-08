
## Princip för generell evaluering av funktioner

En generell princip skulle kunna vara: Maintaina en lista av alla potentiella resultat. När listan bara innehåller resultat av ett enda värde är funktionen konstant.

## Proof that D(f) = D(! . f)

We start by noting that an arbitrary decision tree $t_f$ for a function $f$ is a representative for the function $f$.

We also note that by inverting all the leaf nodes of $t_f$ we instead get a representative for the function $! \circ f$. We denote this tree $t_{! \circ f}$. This trivially follows since any path through $t_f$ that resulted in a $0$ now results in a $1$ in $t_{! \circ f}$ and vice versa. Note that since we have only changed the values of the leaf nodes, the trees will still have the same structure.

With this, we have shown that for any given decision tree for $f$ we can create a corresponding decision tree for $! \circ f$ where the trees have the same structure. When calculating the complexity we only care about the structure of the decision trees, not the actual result in the leaf nodes. This is easily shown by the definition of res for polynomials:

`resPoly :: Ring a => Bool -> a`
`resPoly _b = zero`

where we see that the value is ignored.

Thus, the functions will have the same complexity, since they are represented by decision trees that are identical with regards to structure.

QED.

## Proof that D(f) = D(f . flipAllInputs)

Spegling

## Proof that D(f) = D(f . flipSingleInput)

## Observations

Tänk på det som att det setBit av en 3-bitars funktion ska returnera inte är en ny 3-bitars funktion med en bit fixerad, utan faktiskt en 2-bitars funktion. 
	- setBit ska inte användas för att sätta en bit i en specifik funktion, utan för att röra sig till en ny ekvivalensklass av funktioner.

## Threshold funs

All basic threshold funs are iterated threshold funs, but not the other way around.

## Comparison of BDDs

We're still unsure about whether the equality test for BDDs is correct. We have not yet encountered an error using quickcheck.