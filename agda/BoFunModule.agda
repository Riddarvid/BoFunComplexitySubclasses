module BoFunModule where

open import Agda.Builtin.Nat
open import Agda.Builtin.Equality
open import Data.Bool
open import Data.Vec
open import Data.Fin
open import Data.Fin.Subset using (Subset; _∈_; _⊂_; ⁅_⁆) renaming (_-_ to _S-_; ⊥ to S⊥)
open import Data.Fin.Subset.Properties

variable
  n m : Nat

-- Inductive definition of DecTree
data DecTree (I : Subset n) : Set where
  res : Bool -> DecTree I
  pick : (i : Fin n) -> {i ∈ I} -> DecTree (I S- i) -> DecTree (I S- i) -> DecTree I

BoFunInput : (I : Subset n) -> Set
BoFunInput {n} I = (i : Fin n) -> (i ∈ I) -> Bool

BoFunSemantics : (I : Subset n) -> Set
BoFunSemantics I = BoFunInput I -> Bool

-- We now need a way to translate from DecisionTree syntax to semantics,
-- that is, we need to go from DecTree I to function from boolean input to bool.
treeToBoFun : {I : Subset n} -> DecTree I -> BoFunSemantics I
treeToBoFun (res b) _ = b
treeToBoFun {_} {I} (pick i {i∈I} dtF dtT) input = if input i i∈I 
  then treeToBoFun dtT input2 
  else treeToBoFun dtF input2
  where
    input2 : BoFunInput (I S- i)
    input2 j j∈I-i = input j j∈I
      where
        j∈I : j ∈ I 
        j∈I = p─q⊆p I (⁅ i ⁆) j∈I-i

fiInJ : {I : Subset n} -> {J : Subset m} -> (I -> J) -> Set
fiInJ {n} {m} I J = {! {i : Fin n} -> {j : Fin m} -> {i ∈ I} -> {j ∈ J} ->  i ∈ !}

-- mapDT : 
--   {I : Subset n} 
--   {J : Subset m} 
--   {i : Fin n}
--   {j : Fin m}
--   -> (Subset n -> Subset m) -> DecTree I -> DecTree J
-- mapDT i2j (res b) = res b
-- mapDT i2j (pick i {i∈I} dtT dtF) = pick (i2j i) {{!   !}} (mapDT i2j dtT) (mapDT i2j dtF)

-- mapBF : {I : Subset n} {J : Subset m} -> (Fin n -> Fin m) -> BoFunSemantics I -> BoFunSemantics J
-- mapBF = {!   !}

mapDT : {!   !}
mapDT = {!   !}

mapBF : {!   !}
mapBF = {!   !}

----------- Actual proofs -----------------------



-- TODO add proof
--data Injection (A B : Set) : Set where
--  inj : (A -> B) -> A injprop B -> Injection A B 

dir1 : 
  {I : Subset n} 
  {J : Subset m} 
  {tj : DecTree J} 
  {fj : BoFunSemantics J}
  {ti : DecTree I}
  {fi : BoFunSemantics I}
  -> (i2j : Fin n -> Fin m) -> (treeToBoFun tj ≡ fj) -> {tj ≡ mapDT i2j ti} -> {fj ≡ mapBF i2j fi} -> (treeToBoFun ti ≡ fi)
dir1 = {!   !} 