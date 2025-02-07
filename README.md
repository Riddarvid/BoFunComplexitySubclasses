# exploring-level-p

Associated code for the paper "Exploring level-p-complexity for subclasses of Boolean functions" by Arvid Rydberg and Selina Engberg. The paper is currently not published but a working version can be found at <https://www.overleaf.com/read/zysfktzvrhgf#d16761>

The code builds upon the library presented in the paper "Level-p-complexity of Boolean Functions using Thinning, Memoization, and Polynomials" (published in JFP as [https://doi.org/10.1017/S0956796823000102](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/levelpcomplexity-of-boolean-functions-using-thinning-memoization-and-polynomials/58122B71C40F99E0D19ACD0FAFF867A9#article)) by Julia Jansson and Patrik Jansson, as well as the extensions added by Christian Sattler.

## Overview

The project contains the following modules:

- BDD: Functionality and utilities regarding binary decision diagrams.
- Complexity: Code regarding complexity computations.
- Exploration: Various functionality for exploring Boolean functions and their complexities.
- Poly: Code regarding polynomials and piecewise polynomials.
- Subclasses: Representations of various subclasses of Boolean functions.
- Testing: QuickCheck generators and properties.
- Algebraic: Algebraic numbers.
- Arity: Typeclasses regarding function arity.

In addition to the Haskell code, the python script bench.py can be used to benchmark complexity calculations. The reason for this being done via python is that memoization interferes with native benchmarking. In order to use the script, one of the functions defined in Exploration.Measurements.IO must be called from main.

## Defining a Boolean function

Boolean functions are represented by the typeclass BoFun, found in the module Complexity.BoFun.
The typeclass has three functions:

- isConst: Given a BoFun, determines whether the BoFun is constant, and in that case which value it returns.
- variables: Given a BoFun, returns the list of variables that the function is defined over.
- setBit: Given a variable index, a value, and a BoFun, applies the BoFun to the value.

Using this, we can now calculate the complexity of the BoFun.

## Computing the level-p complexity of a Boolean function

The library exposes three separate but related algorithms for calculating the level-p complexity of a BoFun.

### Complexity.GenAlg.genAlg

The genAlg function was originally presented in the paper by Julia Jansson and Patrik Jansson.

This function can be used to compute the complexity of any BoFun, and returns a Data.Set of any type belonging to the Algor typeclass. Specifically, Janson & Janson's library gives instances of Algor for polynomials and decision trees, as well as tuples of Algors.

This algorithm is very general and can easily be enhanced with thinning and memoization. Shorthands for these are given in the functions genAlgMemo genAlgThinMemo, from the Complexity.GenAlg module.

### Complexity.Piecewise.complexity

The complexity function was originally introduced by Christian Sattler under the name computeMin.

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

### Threshold functions

A threshold function is a function yielding 1 when the number of 1's in the input is reached or exceeded, and 0 otherwise. Another way to look at it is that a threshold function is guaranteed to yield 0 when a certain number of 0's have been read from the input, and 1 once a certain number of 1's has been read from the input. This is the representation used internally, a tuple containing the number of 1's needed for a result of 1 (nt), and the number of 0's needed for a result of 0 (nf). A threshold function can be constructed as follows:

```Haskell
let thresholdFun = ThresholdFun $ Threshold (nt, nf)
```

From the definition, we can see that the order of input bits is not relevant to the result, meaning that the function is symmetric. Thus, it makes sense to index the variables of the function with the type (), since there is no difference between variables. However, when it comes to the concepts of lifting and iterating, introduced later in this document, we need a way to differentiate between input bits. Thus, the module also exposes a version of ThresholdFun (called NonSymmThresholdFun), which is instead indexed using Ints.

Using a pattern synonym, a NonSymmThresholdFun can be constructed as follows:

```Haskell
let thresholdFun = NonSymmThresholdFun $ Threshold (nt, nf)
```

Internally, however, a NonSymmThresholdFun is just a wrapper for a ThresholdFun with a slightly modified BoFun instance.

### Symmetric functions

As mentioned previosly, a symmetric function is a function whose result does not depend on the order of its input bits. This means that we essentially have a mapping from number of 1's in the input to results. Since the internal representation is a bit convoluted, we instead expose the function mkSymmetric that takes a non-empty list of Bools and returns a SymmetricFun. The result if the input contains 0 1's is the first element of the list, the result if the input contains exactly 1 1 is the element at index 1 and so on. For example, a 5-bit function yielding 1 only if the number of 1's in the input is even can be constructed as follows:

```Haskell
let evenSymmetricFun = mkSymmetric (True :| [False, True, False, True, False])
```

As with threshold functions, this type of function can be indexed over (), but we also expose the type NonSymmSymmetricFun indexed over Ints, to be used together with lifitng and iterating.

### Gate functions

Gate functions are a fairly simple class of boolean functions. They are simply the 1- and 2-bit functions that can be represented by the Boolean operators And, Or, and Not. The gates are represented by a constructor each.

As with threshold and symmetric functions, two variants of Gate exist, one optimizing using its symmetric property and one that doesn't in order to be compatible with lifting and iterating.

### General functions

General functions, or just functions, are represented internally using a data structure called a binary decision diagram, or BDD. A BDD can be thaought of as a compressed truth table. These can be constructed in a number of ways, but the simplest way is to construct them using a combination of boolean expressions. The library also supports creating a BDD from a mapping from Ints to Bools, representing the results for rows of a truth table. These two methods are shown here:

```Haskell
let f = x1 ^ x2

let f' = bddFromOutputVector 2 $ IntMap.fromList [(1, False), (2, False), (3, False), (4, True)]
```

f and f' are identical.

Three variants of the general function representation exist, corresponding to two distinct optimizations of general functions, as well as the combination of the two optimizations.

The first optimization is what we call normalization. When setting a bit of an n-bit function, this version removes any redundant variables in the BDD, and maps the remaining variables so that they only use the variables [1 .. n]. This operation does not change the complexity, which is the reason why we can do this optimization. This leads to much greater utilization of memoization, as any functions with the same normal form can now use previously computed results. This representation is found in Subclasses.GenFun.NormalizedGenFun.

The second optimization is similar to the first one, but uses the fact that inverting the output of a function does not change the complexity. This means that we can define a function as being in canonical form iff an input consisting only of 0's yields 0. Otherwise, we can transform the function into canonical form by inverting the output. In testing, this optimization has not had much effect on the total time of the complexity calculations. This representation resides in Subclasses.GenFun.CanonicalGenFun.

Finally, Subclasses.GenFun.NormalizedCanonicalGenFun is the combination of both optimizations.

### Combining BoFuns

As well as these classes, the library provides a way of combining and iterating Boolean functions over eachother. These are exposed in the Subclasses.MultiComposed.MultiComposed and Subclasses.MultiComposed.Iterated modules.

Lifting a function f over a list of functions gs basically means replacing each variable of f with a subfunction from gs. For example, if a 3-bit function is lifted over a 2-bit function, a 5-bit function and a 4-bit function, the resulting function would be a 11-bit function.

Iterating a function takes the concept of lifting and repeats it, creating a tree-like structure of functions over subfunctions over subfunctions and so on, until either a constant function or the identity function is reached.

Lifting is done via the pattern Lifted, defined in Subclasses.MultiComposed.MultiComposed. Iterating a function over a list of iterated functions is done via the Iterated pattern, defined in Subclasses.MultiComposed.Iterated. This module also exposes the Id and Const constructors, facilitating the construction of iterated functions.

Another way of constructing an iterated function is to iterate a function over itself a set number of times. For example, 3 level iterated 3 bit majority is constructed by first replicating maj3 3 times, then lifting maj3 over this list of 3 functions. The process is then repeated until the desired number of levels is achieved. This functionality is expressed in the function iterFun in the Subclasses.MultiComposed.Iterated module.

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

## Exploration of complexities

The folder Exploration contains various tools useful for examining and exploring Boolean functions and their complexities. Here is a short description of each of the modules:

- Critical - contains functions for determining the number of extreme points of BoFuns, specifically in the interval [0, 1].
- Eval - contains functions for evaluating BoFuns for given inputs.
- Filters - contains predicates useful for filtering lists of complexities.
- Measurements - contains functionality for measuring the time it takes to compute the level-p complexity for BoFuns. The caller can specify the number of samples, as well as the number of different functions that should be tested.
- PrettyPrinting - pretty printing of functions and polynomials.
- Translations - functions used for translating between different representations of the same function. Very inefficient as of now.

## Testing

A number of QuickCheck properties and generators can be found in Testing.Properties. These can either be run individually, or the function testAll in Testing.AllProperties can be used to run all properties.
