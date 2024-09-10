module BDD (
  BDDFun,
  pick,
  bddAsc,
  bddDesc
) where
import           Data.DecisionDiagram.BDD (AscOrder, BDD, DescOrder, ItemOrder,
                                           ite, var)

type BDDFun o = BDD o

pick :: ItemOrder a => Int -> BDDFun a -> BDDFun a -> BDDFun a
pick i a0 a1 = ite (var i) a1 a0

bddAsc :: BDDFun AscOrder -> BDDFun AscOrder
bddAsc = id

bddDesc :: BDDFun DescOrder -> BDDFun DescOrder
bddDesc = id
