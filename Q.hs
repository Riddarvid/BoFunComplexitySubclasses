

1) complexity does not change from I to I\{i}
  1a) best cost is when flatten dt = "touched part of I"

  1b) Om J delmängd I, och f' : BF J, f : BF I och f ~= f', då är complexity the same (even cost).

    f ~= f' =  f == f' . mapBF e

mapBF : (i->j) -> BF i -> BF j
mapBF e f = \jtup -> f (comap e jtup)
mapBF e f = f . comap e

comap :: (i->j) -> Tup j -> Tup i
comap e tup = \i -> tup (e i)
comap e tup = tup . e


prop1 :: (Ind i, Ind j) => (i -> j) -> DT i -> Tup j -> Bool
prop1 e t tup = injective e ==>
  (cost t (comap e tup) == cost (mapDT e t) tup)

  t  = [b1,b2,b3,b4,b5]
  t' = [   b2,   b4   ]  -- bara kvar det som spelar roll för (den bofun vi pratar om)

  let t' = mapDT e t

-- t               :: DT i
-- t' = mapDT f t  :: DT j
-- cost t          :: Tup i -> Nat
-- cost t'         :: Tup j -> Nat
-- tup             :: Tup j
-- tup . f         :: Tup i

   

2) complexity is not affected by bijections


type Prob = Real
type Poly = Prob -> Real

complexity är minimum av (expect . cost) över alla DecTrees.

expect : Cost i -> Poly  -- en linjär funktion av (c i)
expect c p = sum [ptup * c tup | (p,tup) <- allTuplesWithProb p]

-- 2 bitar
allTuplesWithProb : Prod -> [(Prob, Tup i)]
allTuplesWithProb p = [(p*p,[T,T]), (p*(1-p),[F,T]), ((1-p)*p,[T,F]), ((1-p)*(1-p),[F,F])]

Om vi har c1 och c2 : Cost i och de är lika så är (trivialt)
  expect c1  ==  expect c2


type Tup i = i -> Bool
type Cost i = (Tup i -> Nat)


cost : DT i -> (Tup i -> Nat)
cost = foldDT costRes costPic

costRes : Bool -> (Tup i -> Nat)
costRes _b tup = 0

costPic : I -> (Tup i -> Nat) -> (Tup i -> Nat) -> (Tup i -> Nat)
costPic i c0 c1 tup = 1 + 
  if index tup i then
    c1 tup'
  else
    c0 tup'
 where tup' j =
         if i>=j then
           index tup (j+1)
         else
           index tup j
  
                        

----------------------------------------------------------------
Iterated är under antagandet att inga bitar används i fler än en delfunktion.
    (vilket gör att när man går ned i "expression tree" vet vi snabbt att många bitar inte längre kan påverka resultatet, dvs. J mycket mindre än I.)

----------------
typen på setBit?

en kombination av  
  setBit : I -> Bool -> BF I -> BF (I \ {i})
och (för injektiva index-funktioner)
  mapBF : (I -> J) -> BF I -> BF J


(I praktiken syns inte typerna I och J, eller möjligen som Fin n och Fin m med n>m.)


