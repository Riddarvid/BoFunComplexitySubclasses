* Q: how can we evaluate a polynomial q at an algebraic number a represented by the triple  (l,p,h)?
+ Types: l, r are rational, p is a (rational) polynomial.
+ I will assume it is enough to know the sign (-1, 0, or 1) of the
  answer (inspired by the evaluation of derivatives at the
  intersections between polynomial pieces.)
#### Step 0: If a is rational m/n, it is immediate: just evaluate q on m/n.
+ The rest talks about when a is not rational.
#### Step 1:
+ let I = [l,h) be an interval such that p has exactly one root in I.
+ this root is a (but we don't have any explicit expression for it)
+ thus we know
  eval p a == 0
+ and also
  (eval p l)*(eval p h) == -1
+ (We can also choose to have (eval p l) < 0 and (eval p h) > 0, otherwise just switch sign on p.)
+ and also
  numRoots p I == 1
#### Step 2: Is q a == 0?
+ Background: If q a == 0 then (x-a) is a common factor is both p and
  q, and thus also a common factor in gcd p q.
+ Compute
  r = gcd p q
  nr = numRoots r I
+ If nr == 0, then q is non-zero in and has the same sign in the whole
  interval. We get sgn (q a) == sgn (q l) and we can get an interval
  for the answer by J = [q l, q r) or the opposite.
+ If nr == 1 (there is just one root of r), then this root must also
  be a root of p, which only has one root: a. Thus p a == q a == r a
  == 0.
+ (If nr > 1 that is impossible.)
