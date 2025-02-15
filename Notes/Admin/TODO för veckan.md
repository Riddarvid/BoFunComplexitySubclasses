


Här är början på mina kommentarer - jag återkommer när jag hunnit vidare.
/P
* 2025-02-14: Reading final report
+ Abstract:
  + ~~"Jansson[1]" := "Jansson [1]" - leave a (non-breaking) space~~
    ~~before \cite{...} in general.~~
  + "mem-oization" := "memo-ization" (line break)
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
+ p5: citation [11] looks a bit strange (on p.59): "E. of Mathematics"
  := "Encyclopedia of Mathematics"
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
 
 
 
 
 
 
 
 
