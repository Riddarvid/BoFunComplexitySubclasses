Done:
## Abstract

* To get rid of the ugly line breaks such as before "memoization", add hyphenation before the document: \hyphenation{me-mo-i-za-tion}.
  Similar for level-p-complexity in 4.1.


Introduction: 

1.1:

* Add space before [2].

* Remove comma before "in order to know".

* Computing level-p-complexity: why is it obvious that the least expected cost is a piecewise polynomial in p?
  Maybe break that sentence after "least expected cost" (the obvious part).
  Then say in a new sentence that this forms a piecewise polynomial (citing [1]).
  (I don't see an obvious way of computing the pieces of the piecewise polymomial.)

### 1.2

* Remove comma before "in order to".

### 1.3

* Add space for [10].

* Add period after "section 3.2".


## 2. Theory

### 2.1

* Can you change Figure 2.2 so that the tree is not perfect?

* In Definition 2.1.1, there is a weird amount of vertical space.
  This shows up also in other definitions, for example Definition 2.3.2.

* In mathematical definitions such as Definition 2.1.2, it is unusual to write "if and only if" for the newly introduced term.
  Just "if" is enough.
  But emphasize the term that is being defined: \emph{constant}.

* In "v : (I → B)", the parentheses are redundant.

* Here is a trick to avoid bad line breaks such as between "Boolean functions" and "[2]": write "Boolean functions~[2]".

* Add period after "is an example".

* Fix typo in Definition 2.1.3: "J(I)" should be "J(i)".

* Add hyphen in "multi composed".


* "need to consider 2 bits": "need to consider close to 2 bits on average".


* "to 2.5": "towards 2.5"

* Remove "and can be expressed as a polynomial in p" in the second paragraph:
  You say this again in the third paragraph.
  In fact, remove the paragraph break and continue with "The level-p-complexity of f is then defined as [...]".


* Don't capitalize decision tree in Figure 2.3.

* Don't capitalize concepts such as:
  - binary decision diagram,
  - directed acyclic graph,
  - reduced ordered binary decision diagram
  when they are written out.
  The acronyms can be capitalized.

### 2.3.1

* The symbol D_p before Lemma 2.3.1 was not introduced before.
  Introduce it when you introduce level-p-complexity: "the level-p-complexity D_p(f) of f is [...]".

* In Definition 2.3.6, it should be "J - i" instead of "J - 1".

* In the formula before Lemma 2.3.1, you quantify over every J, but then specify what J is.
  Which one is it?
  Also, you speak of [t], which does not appear in the formula.

* On page 18, it should be "if i ∈ J" instead of "if i ∈ I".

* On page 22, there is a typo in "cost(t₀'), v|_{J₀}".

* On page 19, I don't understand how you go from the third expression to the fourth expression.
  The term
    restrictDomain_DT(J, Pick(i, t₀, t₁), v)
  unfolds to something involving
    restrictDomain_DT(J - i, t₀, v|_{I-i}).
  But t₀' is defined as
    restrictDomain_DT(J₀, t₀, v|_{I-i}).
  I see J₀ ⊆ J - i, but I don't buy J₀ = J - i.
  
* On page 19, you state that J₀ = J₁ = J if i ∉ J.
  I see J₀, J₁ ⊆ J, but I don't buy that they have be to equal.

* Hint to fix the proof of Lemma 2.3.1: generalize the hypothesis.
  The claim holds not just for J the set of variables that [t] depends on.
  More generally, it should be true when J ⊆ I includes all variables that [t] depends on.
  That gives you added flexibility when you apply the induction hypothesis.

* In Lemma 2.3.2, you introduce f, but then don't use it in the statement.
  Do you mean to introduce t and then talk about [t] instead of f?
  Then you can also remove any references to f in the proof.

* In the proof of Lemma 2.3.2, there is a step missing.
  You have LHS = [...] = [t](v) by Lemma 2.3.1 and [t](v) = [t](v') since f is independent from the complement of J in I.

### 2.3.2

- Typo on page 26: "map_{BF}(e, [t_0]".

* Definition 2.3.9 only works for general functions e if you allow decision trees that inspect the same variable several times (changing your definition of those).
  To see this, apply the unique function e : {0, 1} → {0} to a decision tree for AND.
  A quick fix is to restrict to injective functions e.

* Same remark as for Definition 2.3.9 applies for Lemma 2.3.9.

## 3. Method and evaluation

### 3.1

* Typo: "to lookup" should be "to look up".
  There is another occurrence of this in Section 4.2.


### 3.2

* Do not capitalize "symmetric", "general", "threshold", etc.
  You can capitalize "Boolean" (as it is a name).


## 4. Implementation of optimizations

### 4.1

* Instead of

  > We define a Boolean symmetric function as a Boolean function for which [...]

  you can just say

  > We define/call a Boolean function (to be) \emph{symmetric} if [...]

  Similar for other classes.


### 4.3

* Typo: "values value"

## 5. Tools for exploring level-p-complexity

### 5.2

* In:

  > if the algebraic number shares a root with the polynomial q

  You mean *is* a root, meaning that the polynomial for the algebraic number shares a root with q within the appropriate interval.


## 6. Results

### 6.2

* It's a bit strange that you sometimes abbreviate "figure" and sometimes not.
  (If you use cref to generate references, this cannot happen as the name is generated as part of the reference.)



### 6.3

* "improvement around": "improvement of around".


## 7. Conclusion

* "multi-composed and iterated": "multi-composed and iterated boolean functions".






TODO: 
### 2.3.1

* The induction step in the proof of Lemma 2.3.4 has the same problem as for Lemma 2.3.1.
  You rely on J₀ = J - i.

* I think there is a problem in the proof of Theorem 2.3.6.
  In the definition of complexity, we first go over all decision trees for the function and for each of them calculate the average cost over all inputs.
  But in this proof, you first fix an input vector and then look at the decision tree minimizing the cost for that input vector.
  In particular, you may get different decision trees for different input vectors!
## Not addressed

* This is just opinion, but multi-letter math symbols usually look better when they are typeset in mathsf.
  This distinguishes them from multiplied single-letter variables.

* Definition 2.3.3 can be simplified slightly:

    cost(Pick(i, t₀, t₁), v) = 1 + cost(if v(i) then t₀ else t₁, v|_{I-i}).

  If one thinks of t₀ and t₁ as a function B → DecTree(I-i), then this is simply:

    cost(Pick(i, t₀, t₁), v) = 1 + cost(t_{v(i)}, v|_{I-i}).

  One can use this to get rid of many multi-line if-then-else expressions in the following proofs quite bit (you don't have to do this).

* Side note: BF is a functor and Lemma 2.3.7 is a general property of functors (they preserve inverses).
  (You don't have to mention this.)

* You call your process normalization, but does it really give a normal form for boolean functions?
  The order of variables is still arbitrary.
  That is, a given boolean function can have several different normal forms.
  Rather, it seems to be a normal form for BDDs with a chosen variable ordering.
  That is, you are normalizing BDDs with a chosen variable ordering, not Boolean functions.

  Maybe do the following?
  * call an n-bit Boolean function \<something\> if it depends on all of its bits,
  * call a BDD with variable ordering normalized if its semantics (as a Boolean function) is \<something\>.

  More appropriate subsection titles would be "normalized binary decision diagrams" and "canonical binary decision diagrams".

## Frågor

* There is a problem with numbers of roots in intervals in your exposition.
  The function numRootsInInterval does not actually calculate the number of roots in the given interval.
  Rather, it calculates a number K such that N ≤ K and K - N is even (using Descartes' rule of signs).
  Here, N is the number of roots with multiplicities in the given interval.

Stämmer detta? Vi har utgått från Patirk och Julias numRoots

## TODO

Läs igenom hela 5.2 och se att det blir rätt med critical point/root.