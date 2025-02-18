
* 2025-02-14: Reading final report
+ Abstract:
  + ~~"Jansson[1]" := "Jansson [1]" - leave a (non-breaking) space~~
    ~~before \cite{...} in general.~~
  + "mem-oization" := "memo-ization" (line break) - selina: not sure how to fix this?
  + ~~"a large number of ... has been" := "... have been".~~ ~~But I suggest
    you rewrite the sentence a bit to make it more direct, perhaps "A
    large number of properties have been implemented (using
    QuickCheck), to increase the confidence in the correctness of the
    algorithms."~~
+ ToC:
  + ~~"Closed under set bit": I suggest you write setBit here as you~~
    ~~do everywhere else.~~
  + ~~page xi: cut "the algorithm" before "piecewiceComplexity" to fit
    on the line.~~
+ p4: ~~"greater separation" := "better separation"~~
+ p4: ~~"more generalized" := "more general"~~
+ p5: ~~citation [11] looks a bit strange (on p.59): "E. of Mathematics"
  := "Encyclopedia of Mathematics"~~
+ ~~p5: "The majority function" := "Majority functions". I also suggest
  you take this opportunity to introduce maj_n for all odd n.~~
+ p6: Def. 2.1.1: I don't quite understand: you first introduce the
  notation BF(I) for "the type of Boolean functions defined over a set
  of variables 𝐼". But then after "where" you seem to introduce a
  different notation using the same two characters "𝐵𝐹 ∶ (𝐼 → B) → B →
  𝐵𝐹 (𝐼)". If I should read this as a constructor (I call it BFC to be
  clear) this constructor takes two argument: one function of type
  (I->B) which I call a vector and a value of type B (a bit). Then
  (BFC vector bit) is claimed to have type BF I. Perhaps you mean that
  BF I = (I->B) -> B? or that BFC : ((I->B)->B) -> BF I?
+ p7: Why not Haskell mode/style for this setBit?
  𝑠𝑒𝑡𝐵𝑖𝑡 ∶ 𝐼𝑛𝑑𝑒𝑥 → B → 𝐵𝑜𝐹𝑢𝑛 → 𝐵𝑜𝐹𝑢𝑛
  𝑠𝑒𝑡𝐵𝑖𝑡(𝑖, 𝑏, 𝑓) = 𝜆𝑣 → 𝑓(𝑣′ )
    where
       𝑣′(𝑗) = if 𝑖 = 𝑗 then 𝑏 else 𝑣(𝑗)
+ p7: "(𝐼 − 𝑖)" := "(𝐼 − {𝑖})" (if "−" is set subtraction). Also in
  Def. 2.3.1 (and probably elsewhere as well).
+ ~~p7: "functions that" := "functions which" ?~~
+ ~~p7 (same sentence): the \cite placement is confusing - it looks like
  it belongs to "the report" but it is something completely
  different. Please add a few words (perhaps "(from \cite{...})" after
  "functions").~~
+ ~~p7: "is equal to 1" := "is equal to 1" but without the bold on the
  digit (this 1 is a count, not a bit/Bool). Or replace "1" with
  "one".~~
+ ~~p7: "An example of a function that is a threshold function is the
  2-bit AND function, which has a threshold of 2 1’s needed. The 2-bit
  OR function is also a threshold function with threshold 1." :=
  (suggestion) "Two threshold function examples are the 2-bit AND
  function (which has a threshold of two 1’s) and the 2-bit OR
  function (which has threshold one). The previous example, XOR, is
  only symmetric, but not a threshold function."~~
+ ~~p8: "over each other" := "with each other" - also change over to
  with in the following definition 2.1.3.~~
+ p8: There is something wrong with the type of multiCompose and it
  cannot be expressed without dependent types. I'll try to explain:
  multiCompose : BF(I) -> ((i:I) -> BF (J i)) -> BF (DepPair I J)
    where the typing rule for DepPair is:
      (i, j) : DepPair I J   when i : I and j : J i
    (DepPair is also called a Sigma type)
+ p8(near end): "𝐴𝑁𝐷2", "𝐴𝑁𝐷3", "𝐴𝑁𝐷": unequal spacing, but on the
  last line of the page "AND" has reasonable spacing.
+ ~~p10: "expressed as a polynomial" - this is inaccurate. The subfigure
  on the right is the graph of the complexity (which happens to be the
  graph of a polynomial). When I see "expressed as a polynomial" I
  expect something like "x^2+3" so I suggest you reformulate (because
  the graph is nice to have).~~
+ p11: Fig. 2.4: The resolution is a bit low - perhaps you can find a
  better version? -NOPE, THERE IS NO BETTER (Cries inside)
+  ~~p11: "Specifically, There" := "Specifically, there"~~
+ ~~p12: "earlier computations. [18]" := "earlier computations [18]."~~
+ ~~p13: "algorithm; As there" := "algorithm. As there" or some other
  reformulation.~~
+ p15: "𝐼 − 𝑖" := "𝐼 − {𝑖}" in a few subscripts
+ p16: Def. 2.3.4: I think you got the definition backwards: "if, for
  all input vectors 𝑣, 𝑓(𝑣𝑖→0 ) = 𝑓(𝑣𝑖→1)" then the function is
  *independent* of the bit (index) i.
+ p16 (and earlier): when writing "long" (more than one chacter) names
  in math mode you need to surround the name with \mathit{} or similar
  to get the spacing right. (Most visible with AND_2 and friends
  earlier, but also visible for restrictDomain here.)  Easiest may be
  to create a few LaTeX macros and then add backslash in front of such
  names. Example:
  \newcommand{\AND}{\ensuremath{\mathit{AND}}}
+ p16: def of restrictDomain: then and else need to be indented
+ p18: The LHS calculation is unnecessarily long and thin. I suggest
  to let at least the first if statement fit on one line. (I made such
  a change in overleaf as a suggestion.)
+ p19: The length makes this hard to read - please first introduce a
  few names - such as your
    
    t_0' = restrictDomain_{DT}(J - i, t_0, v\vert_{I - i})\\
    t_1' = restrictDomain_{DT}(J - i, t_1, v\vert_{I - i})\\

  and then just use them without the "where-clauses".
+ p19: "Note that since i notElem J" - I don't quite understand where
  this is used? On the line of the comment, no J is mentioned.






  ~~+ p4: "regaring" := "regarding"~~
+ p19: In general, try to use names of subexpressions in such a way
  that the reader can easily spot what is changed in each step of the
  equality chain. Right now it becomes a blur of symbols for me.
+ p20: RHS = restrictDomain on two consecutive lines
+ p21: "𝐿𝐻𝑆 = 𝑅𝐻𝑆, so we have proved the base case." := "𝐿𝐻𝑆 >= 𝑅𝐻𝑆, so we have proved the base case."
+ p22: Again, avoid repeating the def. of short names (t0' and t1')
+ p22: "In the first branch ...": I don't understand this reasoning.
  In the first branch we still have cost(t,...) which is not what the
  IndHyp talks about. (It only talks about the smaller t0 and t1.) On
  the other hand, this first branch is very similar to the LHS, so
  perhaps no IndHyp is needed here (some other lemma?).
+ ~~p23: "Boolean input vector" := "vector" (to make the line fit)~~
+ p23 "Together, ...": I cannot follow this. Why the negative "cannot
  have lower" structure? I find it hard to understand. Can it be
  formulated in the positive direction? Some facts: choose a v. Then
  let c t = cost (t,v)
  let t* = minarg c
  Then c t* <= c t' for all t' (by def. of minarg)
  Then combine with the lemmas to get an inequality chain.
+ p24: You seem to define mapping for all functions e : I->J, not only
  for bijections, and this works fine. But then the property
    ∀ 𝑓 ∶ 𝐵𝐹 (𝐼), 𝑒 ∶ (𝐼 → 𝐽 ). 𝐷𝑝 (𝑓) = 𝐷𝑝 (𝑚𝑎𝑝𝐵𝐹 (𝑒, 𝑓))
  does not hold. You need to add "e bijective =>".
+ (p25: I think this holds as is, thus for all functions e, not only
  bijections.)
+ p26: last step: indent then and else.
+ p27: I think these if-then-else's would fit without line breaks (and
  be more readable thus).
+ p28: indent then and else
+ p29: 2.3.10: add "bijective e =>"
+ ~~p29: "any Boolean" := "a Boolean" (to fit on one line)~~
+ p29: "By lemma 2.3.7, we have 𝑓 = 𝑚𝑎𝑝𝐵𝐹 (𝑒−1 , 𝑓)." :=
       "By lemma 2.3.7, we have 𝑓 = 𝑚𝑎𝑝𝐵𝐹 (𝑒−1 , g)."
       (Note the g near the end.)
+ p29: Again I don't understand why you formulate the last paragraph
  with "double negation" (cannot have lower) instead of positively /
  directly.
+ ~~p32: an almost empty page - perhaps the flame graph could be shrunk
  to fit here? Or change the LaTeX instructions so that the text on
  p34 continuous on here.~~
+  ~~p34/35: For readability, I think a numbered list would be good, and
  then in the "As stated before" paragraph on p35, you can refer to 1,
  2, 3, 4. It would also be good to start with the positive: you do
  cover 1, 2, and then say what you will not do (3, 4).~~
+ ~~p35: "rendering all bottlenecks" := "rendering some bottlenecks"~~
+ ~~p35: "QuickCheck properties that can be easily run for different
  parts of our implementation and the implementation as a whole were
  implemented, in order to verify the correctness of our
  implementation [10]." I feel this sentence in unnecessarily
  complicated. Perhaps something like: "To ensure correctness of our
  algorithms we have developed QuickCheck properties and generators
  \cite{...}."~~
+ ~~p35: Don't end a page with a header (Normalization)~~ - currently we do not but check before final submission!!
+ ~~p35: "verrtical" : spelling~~
+ p36: "propNormalizedComplexity" - please also refer back to the
  corresponding lemma / proof.
+ ~~p37(line 1): Help LaTeX with a line-break near level-p-complexity~~
+ ~~p37: "The implementation of this class" := "The representation of
  this class". (In general I think you use "implementation" a bit too
  often.)~~
+ p37/38: The reasoning here feels a bit convoluted. Perhaps instead
  you can use this chain of equalities (of Booleans):
  
    nt >= kt
  == {Invariants}
    (n-nf) >= (n+1-kf)
  == {subtract n}
    -nf >= 1-kf
  == {multiply by -1}
    nf <= kf-1
  == {integer property}
    nf < kf
  == {negated ordering}
    not (nf >= kf)

+ ~~p38: suggestion: "a lot of time is needed to compare" :=
  "significant time is spent on comparing"~~
+ ~~p38: "eachother" := "each other"~~
+ ~~p38/39: Good example with f and g being different functions but with
  the same complexity. But the figure (4.1) names them g and h (and
  uses f for the "normalized" version). Please use consistent naming
  (and perhaps just replace the sentence on p38 with a reference to
  the figure).~~
+ I think the Fig. 4.1 would be helpful already in the section on
  properties and proofs (to illustrate I, J, etc. with an example). So
  perhaps move it much earlier and then refer to it here. Or mention
  it in the earlier section and keep it here.
+ ~~p39: Step 1: The examples here are a bit confusing: why is g' using
  x2 and x3?? and h' using x1 and x2?? Perhaps you meant to swap the
  names?~~
+ p39: Step 2: As you use f' and g' (and f'' and g'') in the
  explanation, it may be good to add them to the figure as well. That
  would make your two steps easier to follow.
+ ~~p39/40: "However, this would likely require a complete rebuild of
  the BDD, as we would have to preserve variable ordering." :=
  "However, this would likely require a complete rebuild of the BDD,
  if the variable ordering is not preserved."~~
+ ~~p40: "simply map over the structure of the BDD" := "simply rename
  the variables in the BDD"~~
+ ~~p40: "The concrete implementation of this consists of creating a
  newtype NormalizedGenFun wrapping GenFun": This is the first mention
  of "GenFun" in the whole report. (This may surprise/confuse the
  reader.) You may not need to explain it to this level of detail? So
  far I think the reader would (implicitly) expect the "GenFun" type
  to be named "BDD" or so.~~
+ ~~p40. §4.3.3: Mention early that the complexity is unchanged by
  inversion.  (Otherwise this subsection will seem mysterious.)~~
+ ~~p41: Heading: Ugly line-break. I suggest you reformulate to make it
  a bit shorter. Perhaps just skip the word "Finding" and replace
  "for" by "of".~~
+ ~~p41: §5.1: The start of this section does not really deal with
  finding critical points, but more generally comparing polynomials. I
  suggest you make a new subsection "Comparing polynomials" before the
  current §5.1.~~
+ ~~p42: bullet 2. "criticalType": LaTeX needs help with the line-break.~~
+ ~~p42: "can be observed" := "can be found" or "is defined" (also on
  p43, twice, and on p45)~~
+ ~~p43: "it adds a minimum at 0 as well."?? not a maximum?~~
+ ~~p45: When this rather long section section (§4.1) ends, the reader
  is really interested in seeing at least one or two examples. If they
  appear later, please add a forward pointer, otherwise add them. A
  nice example could be the complexity from the front page which has a
  nice mix of the three types of (local) extrema.~~
+ ~~p49: "explicitpiecewiseComplexity" :=
  "explicitPiecewiseComplexity" - there currently seem to be several
  uses of both capitalizations in the document. Please unify.~~ - I changed to explicitpiecewiseComplexity throughout the report
+ ~~p51: A "spike" is, at least for me, a value which is significantly
  higher than both values on the left and on the right. But you seem
  to mean something else, but I cannot find a single "spike" in the
  graph in Fig. 6.3. Please reformulate. (sudden increase?, although I
  cannot see that either)~~
+ p51: It is a bit hard to read when Fig. 6.3 is two pages away from
  the text. - Selina: do not see any good way to fix this and for it to still look nice. 
+ ~~p52: "IQR" - spell out (what is it?)~~
+ p57: "While adding canonicalization": around here you move from
  specialized to general optim. which, I think, deserves a more
  visible transition. Perhaps a \paragraph{Specialized classes} at the
  start of paragraph 2 and \paragraph{General optimizations} at the
  start of this paragraph? (and perhaps label the last paragraph as
  well) - Selina: I disagree. we do not have enough text to split into sections
+ ~~p57: "𝑓(𝑥1 , 𝑥2 ) = 𝑥1 ∧ 𝑥2": please use subscripts also on the RHS.~~
+ ~~p57: "𝑔(𝑥1 , 𝑥2 ) = 𝑥2 ∧ 𝑥3": confusing function: x3 is not in
  scope. (and please use subscripts also on the RHS)~~
+ ~~p57: "a lot gain" := "a lot of gain"?~~
+ ~~p60: "G. Team" := "GHC Team" - probably needs {GHC Team} or so in
  BibTeX.~~
+ ~~pI: A.1: You need a sentence or two to explain what a, b, c,
  etc. mean. (... polynomial pieces such that \x -> min [a x,b x,c
  x,...]  is the complexity function.)~~
+ ~~pIV: A.3 Code listings: Avoid lonely headings without contents. At
  least make sure it comes after Figure A.6.~~
+ ~~pV: Nice diagram!~~
+ pVI: rename "_low_" to "low" or something similar. Haskell praxis is
  to use initial underscores only when a variable name is _not_ used
  on the RHS.
+ ~~pVI: criticalPointBetweenPieces or criticalPointsBetweenPieces ? The
  listing caption has one name, the function has another.~~
 
 
 
 
 
 
 
 
