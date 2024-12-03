Vad vi har gjort:

- Hanterar derivata 0
- Writing seminar 2


Bernstein polynomials?

-- Vi generering kan denna bas vara mer användbar är "monomial-basen" x^k
-- Också bara att generera på 0-1 intervallet, sedan "skala om".
-- https://en.wikipedia.org/wiki/Bernstein_polynomial
b n k = "n över k" * p^k * (1-p)^(n-k)

--  alla har grad exakt n
--  k nollställen i 0 och n-k i 1
--  är ändå en bas för vektorrummet P_n = polynom av grad <= n

  (a*p + b*(1-p))^n
=
  \sum_{k=0}^n  a^k * b^(n-k) * b n k

  exempelvis a=b=1

  (p+(1-p))^n = 1^n = 1
  
-- Hypotes: Symmetriskt polynom har symmetrisk koefficientlista
--   [7,5,7]
 