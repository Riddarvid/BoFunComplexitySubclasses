The total number of n-bit boolean functions is 2^(2^n). We can see this if we think about truth tables and the number of permutations of outputs. A truth table for n input bits will have 2^n rows, and therefore there are 2^(2^n) possible permutations of outputs for this table.

## Functions with exactly one 1 in output

Given n input bits, there exist exactly 2^n functions with only one 1 in the output:

000 0
001 0
010 0
011 0
100 0
101 0
110 0
111 1

000 0
001 0
010 0
011 0
100 0
101 0
110 1
111 0

etc.

One [False, True, True, False, True, True, False, False, False, True]

![[Pasted image 20240906154104.png]]

## Symmetric functions

What matters in a symmetric function is that each input pattern with the same number of 1s must give the same result.
For a function with three input bits we get the following truth table, where we assign a letter to each 1-count.

A 000
B 001
B 010
C 011
B 100
C 101
C 110
D 111

This means that all A-rows must give the same result, all B-rows, etc.

As we can see we have 4 different bit-counts for a 3-bit function: 0, 1, 2, 3. Thus, we need to assign 4 values when defining i symmetric 3-bit function. This results in 2^4 different possible functions.

Specifically, iff a function gives the same output for 001, 010, 100 and the same output for 011, 101, 110, then it is symmetric.

Generalizing to n bits, we can see that the maximum number of ones will always be n, and the minimum will be 0, resulting in 2^(n + 1) possible symmetric functions with n bits.

## Monotonic functions

For 2 bits we find that the number of boolean functions is 6, simply by starting with a Hasse diagram where all nodes have truth value 0 and then exploring which nodes can change truth value and still be monotonic.

The same principle can be used for 3 bits, resulting in 20 functions, but it becomes very tedious to do by hand for 4 bits. According to https://en.wikipedia.org/wiki/Dedekind_number, 4 bits result in 168 functions. However, no closed form expression for Dedekind numbers have been found.

## Odd functions

For odd functions, inverting an input pattern results in inverting the output. Thus, the output for the inverse must be dependent on the output of the original pattern. This gives us:

000  A
001  B
010  C
011  D
100 !D
101 !C
110 !B
111 !A

We must now assign a value to each of A, B, C, D, giving us 2^4 functions. We can also see a very clear visual pattern in that the lower half of the outputs must be the inverse of the pattern formed by the upper half, in the opposite order.

Generalizing to n bits, the number of rows in the truth table is 2^n. We only need to assign values to half of these, resulting in 2^n / 2 = 2^(n-1) values that need to be assigned. As each of these can be one of two values, we get 2^(2^(n-1)) odd functions.

## Even functions

This is very similar to odd functions, except we instead get:

000 A
001 B
010 C
011 D
100 D
101 C
110 B
111 A

By the same logic as above, we get 2^(2^(n-1)) even functions. The visual pattern is also similar, the bottom half of outputs is simply a reflection of the top half.

## Same number of 0s and 1s in output

With n bits, we have 2^n possible inputs. We must choose exactly (2^n) / 2 = 2^(n-1) of these to assign 1s to, and the rest will be 0s. This gives us 
$$ \binom{2^n}{2^{n-1}} $$
functions of this class. We have not found a good way to simplify this expression.

## Pure threshold functions

A 000
B 001
B 010
C 011
B 100
C 101
C 110
D 111

Vi kan välja om thresholdet ska vara vid A, B, C, D, alternativt att threshold är högre än 3, funktionen blir då const3 False.

Från detta följer att vi har 5 potentiella punkter att sätta gränsen.

För n bitar finns det n + 1 olika antal ettor vi kan ha (0, 1, ... , n - 1, n). Vi kan sätta vårt threshold vid vilken som av dessa, samt vid n + 1. Detta ger oss totalt n + 2 olika threshold functions.

## Implemented threshold functions

En thresholdfunction består av ett threshold och ett set av funktioner.

För en n-bitars funktion måste summan av nf och nt vara lika med n. Vi måste även ha att summan av arityn av delfunktionerna vara lika med n.

Delfunktionerna måste ha arity > 0, annars skulle de vara konstanta.

Antalet threshold functions kan då beskrivas av antalet sätt att välja threshold multiplicerat med antalet sätt att välja delfunktioner.

Vi kan välja threshold på (n + 1) olika sätt.

Antalet sätt att välja subfunktioner är vid första anblick mer komplicerat. För det första måste vi bestämma antalet sätt att välja arity för de delfunktionerna, sedan måste vi även ta hänsyn till antalet funktioner som finns av den arityn.

Ett uttryck för antalet funktioner borde vara något i stil med:
$$f(0) = 0, f(1) = 4$$
Ty med en bit kan vi endast välja en sub-function med 
$$f(n) = \Sum_{i=1}^{n} 2^{2^i} \cdot f(n-i)$$

Något är fel, vi får för höga tal.

## Other observations

A function can be both symmetric and monotonic: Ex. AND

A function can be symmetric but not monotonic:

00 1
01 1
10 1
11 0

(NAND)

A function can be monotonic but not symmetric:

00 0
01 0
10 1
11 1

(const x y)

## Number of functions

|                    | 2-bit |     | 3-bit |     | 4-bit |     | n-bit              |
| ------------------ | ----- | --- | ----- | --- | ----- | --- | ------------------ |
| Total              | 16    |     | 256   |     | 65536 |     | 2^2^n              |
| Symmetric          | 8     | 50% | 16    | 6%  | 32    | 0%  | 2^(n+1)            |
| Monotonic          | 6     | 38% | 20    | 8%  | 168   |     | Not known          |
| Odd                | 4     | 25% | 16    | 6%  | 256   | 0%  | 2^(2^(n-1))        |
| Even               | 4     | 25% | 16    | 6%  | 256   | 0%  | 2^(2^(n-1))        |
| Same number of 0/1 | 6     | 38% | 70    | 27% | 12870 | 20% | 2^n choose 2^(n-1) |
| Exactly one 1      | 4     | 25% | 8     | 3%  | 16    | 0%  | 2^n                |
| Threshold          | 4     | 25% | 5     | 2%  | 6     | 0%  | n + 2              |


| Type                   | Closed under setBit (Hereditery) |
| ---------------------- | -------------------------------- |
| Generic                | Yes                              |
| Symmetric              | Yes                              |
| Monotonic              | Yes                              |
| Odd                    | No                               |
| Even                   | No                               |
| Same number of 0/1s    | No                               |
| Exactly one 1          | No                               |
| Exactly zero or one 1s | Yes                              |
| Threshold              | Yes                              |
