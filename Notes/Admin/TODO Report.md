- Förtydliga properties
- Använd hls2Tex för code listings
- Lägg in komplexiteten av IterMaj 3 3 i appendix A
- Se om vi vill lägga till "När vi skriver effektiv eller optimerad menar vi alltid med avseende på komplexitetsberäkningar om inget annat sägs". I introduktionen någonstans?
- Förklara de nya box-plotsen när det är dags
- Ta med att vi i nuläget har flera representationer av samma funktion för en given datatyp (ex. Iterated ThresholdFun)
	- Räcker nog med ett kort exempel, typ 3-bit or = 2-bit or + id.
- Skriv om de egenskaper vi har hittat hos komplexiteter (mirroring osv.)
- I samband med våra grafer borde vi ha en hypotes om det de visar. Ex. om vi ser ett linjärt beteende, vad är k och m? Vad betyder det?
	- t.ex. "Kvoten mellan körtiderna ökar exponentiellt"
- Uppdatera rapporten med korrekta properties.
- Skriv klart bevisen
	- Kolla Julias källor och se om det finns bevis där
- Se över om vi vill lägga till saker som inte fungerade i appendix, t.ex. mirroring.
- Critical points
	- Döp om tools
	- 5.1 Förklara hur signAtAlgebraic funkar, gärna med illustration/diagram.
		- I allmänhet är det nog konceptet shrinking som behöver förklaras
	- Se över all text som rör detta, även code listings.

## Latex-grejer

- Se till att använda macros överallt, för \setBit
- Fixa biblography (upper case, too long links)
- Referera till bilden på framsidan 
- Alla kommentarer

## Idéer

- Använd short-circuiting av boolean expressions som mer relaterbart exempel

## From writing seminar: 

Peer Review: 

Too many subsections with short paragraphs. Lack of transition 
- expand on the sections 
- remove a lot of introduction sentences

explain the terms more in the introductions. like in the contribution. expand on the terms. and perhaps add another example (or the maj example) in the introduction.  

need more citations and sources 

Flamegraphs: mostly good, but sometimes the specific functions are hard to see in the graphs. Add also some axis on the diagram

Potential profiling tool: valgrind, used for c but possibly work for Haskell

The representations section may need to be renamed. Perhaps it is better to be Implementations and results

tables should have descriptions below. 

make better listings (so they stand out more)
- Minted



Language: 

use "we" a lot. Especially in the theory. 

section titles - should we capitalize all the words or not? ask Patrik 

we use quotation when we should use bold

add more reflections in the discussion. Right now it is a lot of summary. Put that summary first and then reflections



## FEEDBACK FROM CHRISTIAN: 

* The threshold functions are exactly the monotone symmetric functions.
  Do you observe this somewhere?

* The labels of the definitions 1 and 2 are typeset strangely.
  Use \begin{definition}[Lifting] ... \end{definition}.

* Definition 1 needs to be clarified instead of offloading the precise definition to later running text.
  For example, it does not say if the g_i all share the same input bits or have disjoint input bits (as you want).
  I suggest that you are precise and annotate not just f, but also the g_i and the "lifting" h with their arity.

* Similarly, Definition 2 needs to be clarified.
  What does it mean precisely to repeatedly lift functions of the same type?
  Is every node an n-ary boolean function for the same n or can the n differ?

  If you feel that you cannot do this in this section, you can move the definition to where it makes sense.
  You can still informally describe the notion here, with a reference to the precise definition later on.
  But I think it is better to give a mathematical definition involving functions here and then later the development involving *representations* of those functions.

* Lifting is a strange name for the operation introduced in Definition 1.
  I would call it (multi)composition.
  Mathematically, this is composition in the [multicategory](https://en.wikipedia.org/wiki/Multicategory#Examples) of sets and n-ary functions.

* One suggestion I have for the mathematical exposition is to use finite sets for arities.
  For example, given a finite set I, an I-ary boolean function is a map
    {0, 1}^I ---> {0, 1}.
  This makes it easier to state composition ("lifting").
  Given
    f : {0, 1}^I ---> {0, 1}.
  and for each i âˆˆ I
    g_i : {0, 1}^{J_i} ---> {0, 1}
  with J_i a finite set, we have
    (f âˆ˜ g) : (0, 1}^{âˆ_{iâˆˆI} J_i} ---> {0, 1}
  defined by
    (f âˆ˜ g)(x) = f(g(x_{i,j})_{jâˆˆJ_i})_{iâˆˆI}.
  So f âˆ˜ g is a boolean function of arity âˆ_{iâˆˆI} J_i.
  Since I is finite and J is a family of finite sets, the set âˆ_{iâˆˆI} J_i is again finite.

* According to your definitions, read-once functions are the same as iterated gate functions.
  Worth observing?

* When you discuss binary decision diagrams, you should mention the [reduction aspect](https://en.wikipedia.org/wiki/Binary_decision_diagram#Definition).
  This is needed for uniqueness.
  Otherwise, every binary decision tree would also be a binary decision diagram.
  And there are many binary decision trees with the same variable ordering that represent the same function.

* The typeclass BoFun should be explained:
  * f is a type representing boolean functions,
  * i is the set of possible variable names.
  What do the operations of the type class correspond to in terms of the represented mathematical boolean function?

* The graph in Figure 2.3 does not seem to correspond to the function f you define.
  I don't understand 0 x < 1/2.
  Also, the function itself is called f, not f(x).
  f(x) is the value of the function f at input x.

* "needs at most 2^n bits of information": remove "at most".

* Remember to introduce all the notation used in Section 2.4.
  For example, the notation f^0_i could be introduced earlier to explain what the setBit operation does in the BoFun typeclass.

* I think it is nicer to define *independent* instead of *dependent*.
  Then you have an equality instead of an inequality, which makes it a positive notion (which are more useful).

* "node's": of the node

* Instead of a decision tree A valid for f, just speak of a decision tree A for f.
  Validity is already part of that.
  (Unless you define validity in some unexpected way.)

* In Lemma 2, it is simpler to fix a p at the beginning.
  Then you don't need to reason about functions [0, 1] â†’ R, but only about real numbers.

* > Level-p complexity is dependent only on a functionâ€™s dependent bits

  Can you make this into a precise mathematical statement?

* > The complexity of a function expressed as a BDD is not dependent on
variable numbering.

  This is better stated as: the complexity of a function is invariant under arity bijections.
  Consider finite sets I and J related by a bijection e : I â‰ƒ J.
  Write
    {0, 1}^e : {0, 1}^J â‰ƒ {0, 1}^I
  for the bijection induced by precomposing with e.
  Given a boolean function g of arity J, then g and g âˆ˜ {0, 1}^e have the same complexity.

  In fact, this framework can also be used to express the notion of independence.
  For this, you can consider functions e instead of bijections.
  If e is injective, then g âˆ˜ {0, 1}^e is the boolean function that ignores all the bits not in the image of e.
  So a boolean function f of arity I depends only on some subset J âŠ† I exactly if it factors as f = g âˆ˜ {0, 1}^e for e the inclusion from J to I.
  If e is surjective, then g âˆ˜ {0, 1}^e is the boolean function links some input bits together, forcing them to have the same value.

* The LaTeX rendering of Haskell expressions is inconsistent and sometimes off.
  For example at the end of page 22, the whitespace space gets eaten up by LaTeX.
  I suggest you always use the same typeface and whitespace convention for the same thing.
  Personally, I prefer non-monospace renderings when you have long function names like "shrinkIntervalPoly" (which currently looks like there is a whitespace between neighbouring letters).

* I would write $(n_t, n_f)$ instead of $(nt, nf)$.
  The latter looks like products of n with t and f.
  A more symmetric intuition for this representation is as follows.
  Suppose k_t bits are true and k_f bits are false.
  Then k_t + k_f = n, so (k_t - n_t) + (k_f - n_f) = -1.
  Because these are integers, exactly one of k_t - n_t and k_f - n_f must be non-negative.
  That is, exactly one of k_t â‰¥ n_t and k_f â‰¥ n_f is true.

* In the discussion, I suggest you represent efficiency improvements always as an improvement factor.
  That is more useful than x% less time needed.


