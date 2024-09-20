
A 1-bit only has complexities described by single polynomials

## Smallest n and simplest example of a 2-piece PP

With 2 bits, four functions result in a 2-piece PP. Notably, all of them result in the same PP:

$$
p1(p) = 1 + p
$$

$$p2(p) = 2 - p$$

p1 wins when p < 0.5, p2 wins otherwise.

The functions with this complexity are:

00 0
01 0
10 1
11 0

00 0
01 1
10 0
11 0

00 1
01 1
10 0
11 1

00 1
01 0
10 1
11 1

## Smallest n and simplest example of a 3-piece PP

For functions with 3 bits, their complexities are at most described by 2-piece PPs.

With 4 bits we find many PWs with more than 2 pieces.

So far, the simplest one we've found is composed of

TODO: Lägg in vilken funktion det var.

p1(p) = 1 + 2p + p²
p2(p) = 1 + 3p - p²
p3(p) = 3 - p²

We say that this is simple since it only consists of 2nd degree polynomials.

We are not yet sure whether a simpler PW exists.

## Smallest n and simplest example of a PP with two maxima in (0, 1)

Found by checking the derivatives of adjacent segments.

This one was found for a 4-bit function.

Our method can sometimes give false positives as we use an approximative root finder.

We have found that there are exactly 648 4-bit functions with 2 maxima.

piecewise polynomial in [0, 1]:
+ separated by 0 % 1
+ piece [2,2,1,-1]
+ separated by 1 % 2
+ piece [4,-3,2,1]
+ separated by root of [-2,7,-6,0] between 1 % 2 and 1 % 1
+ piece [2,4,-4,1]
+ separated by 1 % 1

From these, we get 8 distinct complexities:

p(x) = 3 + x^3
q(x) = 4 -3x + 2x^2 + x^3
r(x) = 4 -2x + 2x^2 -x^3

p(x) = 2 + 3x -2x^2 + x^3
q(x) = 4 -5x + 6x^2 -x^3
r(x) = 4 -4x + 6x^2 -3x^3

p(x) = 3 + x^3
q(x) = 4 -5x + 6x^2 -x^3
r(x) = 4 -4x + 3x^2 + x^3
s(x) = 4 -3x + 3x^2 -x^3

p(x) = 3 + x
q(x) = 4 -4x + 6x^2 -2x^3
r(x) = 4 -2x + 2x^3
s(x) = 4 -x

p(x) = 3 + x -3x^2 + 3x^3
q(x) = 4 -4x + 3x^2 + x^3
r(x) = 4 -2x + x^2 -x^3

p(x) = 3 + x -3x^2 + 3x^3
q(x) = 4 -4x + 3x^2 + x^3
r(x) = 4 -3x + 3x^2 -x^3

p(x) = 3 + x -x^2 + x^3
q(x) = 4 -4x + 5x^2 -x^3
r(x) = 4 -3x + 3x^2 -x^3

p(x) = 3 + 2x -2x^2 + x^3
q(x) = 4 -3x + 4x^2 -x^3
r(x) = 4 -2x + x^2 + x^3
s(x) = 4 -x + x^2 -x^3

A problem that we've realized is that the roots separating the functions may not be rational. Therefore, we must convert to working with reals instead.

We have now done more rigorous searching of the 4-bit PPs with 2 maxima. We have arrived at the conclusion that all of these contain at least one piece which is of at least degree 3. We did this by computing the complexities of all 4-bit functions, filtering out those with 2 maxima, counting the degrees of the PPs, finding the minimum, and then filtering on that.
## Smallest n and simplest example of a PP with three maxima in (0, 1)

4-bit function.

TODO: Skriv ner vilken funktion.

piecewise polynomial in [0, 1]:
+ separated by 0.0
+ piece [3.0,2.0,-2.0,1.0]
+ separated by root of [1.0,-5.0,6.0,-2.0] between 0.0 and 0.5
+ piece [4.0,-3.0,4.0,-1.0]
+ separated by 0.5
+ piece [4.0,-2.0,1.0,1.0]
+ separated by root of [0.0,1.0,0.0,-2.0] between 0.5 and 1.0
+ piece [4.0,-1.0,1.0,-1.0]
+ separated by 1.0


![[Pasted image 20240905093302.png]]