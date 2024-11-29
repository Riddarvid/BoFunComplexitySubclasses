# exploring-level-p

Link to report: <https://www.overleaf.com/read/zysfktzvrhgf#d16761>

## Defininga Boolean function

Boolean functions are represented by the typeclass BoFun, found in the module Complexity.BoFun.
The typeclass has three functions:

- isConst: Given a BoFun, determines whether the BoFun is constant, and in that case which value it returns.
- variables: Given a BoFun, returns the list of variables that the function is defined over.
- setBit: Given a variable index, a value, and a BoFun, applies the BoFun to the value.

Using this, we can now calculate the complexity of the BoFun.

## Computing the level-p complexity of a Boolean function

The library exposes three separate but related algorithms for calculating the level-p complexity of a BoFun.

### Complexity.GenAlg.genAlg

The genAlg function was originally presented in the paper "Level-p-complexity of Boolean Functions using Thinning, Memoization, and Polynomials" (published in JFP as doi:10.1017/S0956796823000102) by Julia Jansson and Patrik Jansson.

This function can be used to compute the complexity of any BoFun, and returns a Data.Set of any type belonging to the Algor typeclass. Specifically, Janson & Janson's library gives instances of Algor for polynomials and decision trees, as well as tuples of Algors.

This algorithm is very general and can easily be enhanced with thinning and memoization. Shorthands for these are given in the functions genAlgMemo genAlgThinMemo, from the Complexity.GenAlg module.

### Complexity.Piecewise.complexity

The complexity function in the Complexit.Piecewise module hase the same behaviour as genAlg, with a few exceptions. This function is specialized to the data type PiecewisePoly, representing a piecewise polynomial. This enables an optimization compared to genAlg, as we no longer return a set of polynomials, but rather a single piecewise polynomial, resulting in more efficient comparisons.

The obvious drawback compared to genAlg is that we are limited to working with PiecewisePolys.

### Complexity.Piecewise.explicitCmplexity

The explicitComplexity function is very similar to the complexity function, but memoization is handled explicitly using a HashMap instead of the memoize library. The reasoning for this is that the type BDD, used to represent general Boolean functions, has a method for efficient comparisons, which is not compatible with the memoize library.

## Classes of Boolean functions

A list of representations subclasses of Boolean functions can be found in the Subclasses folder. The time required to calculate the complexity of these representations can be orders of magnitude less than the same function represented as a BDD. The obvious drawback is that they can only represent a subset of Boolean functions. The classes represented are:

- Threshold functions
- Symmetric functions
- Functions constructed using gates
- General Boolean functions

For general boolean functions, specific optimized data types are also defined.

As well as these classes, the library provides a way of combining and iterating Boolean functions over eachother. These are exposed in the Subclasses.Lifted and Subclasses.Iterated.Iterated modules.

Lifting a function f over a list of functions gs basically means replacing each variable of f with a subfunction from gs. For example, if a 3-bit function is lifted over a 2-bit function, a 5-bit function and a 4-bit function, the resulting function would be a 11-bit function.

Iterating a function takes the concept of lifting and repeats it, creating a tree-like structure of functions over subfunctions over subfunctions and so on, until either a constant function or the identity function is reached.

Lifting is done via the pattern Lifted, defined in Subclasses.Lifted. Iterating a function over a list of iterated functions is done via the Iterated pattern, defined in Subclasses.Iterated.Iterated. This module also exposes the Id and Const constructors, facilitating the construction of iterated functions.

Another way of constructing an iterated function is to iterate a function over itself a set number of times. For example, 3 level iterated 3 bit majority is constructed by first replicating maj3 3 times, then lifting maj3 over this list of 3 functions. The process is then repeated until the desired number of levels is achieved. This functionality is expressed in the function iterFun in the Subclasses.Iterated.Iterated module.

```Haskell
let maj33 = iterateFun 3 maj3 3
```

## Examples

Here is an example of how the three main algorithms can be used to calculate the level-p complexity of the 3 level iterated 3 bit majority function:

```Haskell
let maj33 = Gen.iteratedMajFun 3 3

let c1 = genAlg maj33 :: Set (Poly Rational)

let c2 = complexity maj33 :: PiecewisePoly Rational

let c3 = explicitComplexity maj33 :: PiecewisePoly Rational
```

c2 and c3 should be identical, and c1 should represent the same piecewise polynomial as c2 and c3.
