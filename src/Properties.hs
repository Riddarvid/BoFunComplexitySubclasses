module Properties (
  propComplexityNot
) where
import           Algorithm.GenAlgPW       (computeMin)
import           BDD                      (BDDFun)
import           BDD.BDDInstances         ()
import           Data.DecisionDiagram.BDD (notB)
import           Test.QuickCheck          (Property, (===))

-- Currently becomes very slow with more than 4 bits, so the arbitrary instance
-- for BDD funs is limited to max 4 bits.
propComplexityNot :: BDDFun -> Property
propComplexityNot f = computeMin f === computeMin f'
  where
    f' = notB f
