{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE TemplateHaskell       #-}
module Subclasses.Gates (
  BasicGate(..),
  Gate,
  var,
  notG,
  andG,
  orG
) where
import           BoFun                 (BoFun (..), Constable (mkConst))
import           Control.Monad.Free    (Free (Free, Pure))
import           Data.Function.Memoize (deriveMemoizable)
import           Subclasses.Iterated   (Iterated)

data BasicGate = BAnd | BOr | BNot | BId | BConst Bool

instance BoFun BasicGate () where
  isConst :: BasicGate -> Maybe Bool
  isConst g = case g of
    BConst val -> Just val
    _          -> Nothing
  variables :: BasicGate -> [()]
  variables g = case g of
    BConst _ -> []
    _        -> [()]
  setBit :: ((), Bool) -> BasicGate -> BasicGate
  setBit (_, val) g = case g of
    BAnd     -> if val then BId else BConst False
    BOr      -> if val then BConst True else BId
    BNot     -> BConst $ not val
    BId      -> BConst val
    BConst _ -> undefined

-------------- Iterated gates -------------------------

data Gate f = And f f | Or f f | Not f | Const Bool

$(deriveMemoizable ''Gate)

instance BoFun (Gate (Iterated Gate)) (Int, [Int]) where
  isConst :: Gate (Iterated Gate) -> Maybe Bool
  isConst g = case g of
    Const val -> Just val
    _         -> Nothing

  variables :: Gate (Iterated Gate) -> [(Int, [Int])]
  variables g = case g of
    And g1 g2 -> v2 g1 g2
    Or g1 g2  -> v2 g1 g2
    Not g1    -> v1 g1
    Const _   -> v0
    where
      v0 = []
      v1 = mkVar 1
      v2 g1 g2 = mkVar 1 g1 ++ mkVar 2 g2
      mkVar n g' = map (\v -> (n, v)) (variables g')

  setBit :: ((Int, [Int]), Bool) -> Gate (Iterated Gate) -> Gate (Iterated Gate)
  setBit v g = case g of
    And _ _ -> setBitBinary v g
    Or _ _  -> setBitBinary v g
    Not _   -> setBitUnary v g
    Const _ -> undefined

setBitUnary :: BoFun f i => ((Int, i), Bool) -> Gate f -> Gate f
setBitUnary ((_, i), val) (Not f) = case isConst f' of
  Nothing  -> Not f'
  Just res -> Const $ not res
  where
    f' = setBit (i, val) f
setBitUnary _ _ = undefined

setBitBinary :: ((Int, [Int]), Bool) -> Gate (Iterated Gate) -> Gate (Iterated Gate)
setBitBinary ((i, j), val) g = case isConst f' of
  Nothing -> if i == 1 then c f' f2 else c f1 f'
  Just res -> case (i, g, res) of
    (_, And _ _, False) -> Const False
    (1, And _ _, True)  -> idHelp f2
    (2, And _ _, True)  -> idHelp f1
    (_, Or _ _, True)   -> Const True
    (1, Or _ _, False)  -> idHelp f2
    (2, Or _ _, False)  -> idHelp f1
    _                   -> undefined
  where
    c = case g of
      And _ _ -> And
      Or _ _  -> Or
      _       -> undefined
    f' = setBit (j, val) f
    f = if i == 1 then f1 else f2
    (f1, f2) = case g of
      And f1' f2' -> (f1', f2')
      Or f1' f2'  -> (f1', f2')
      _           -> undefined

instance Constable Gate where
  mkConst :: Bool -> Gate g
  mkConst = Const

idHelp :: Iterated Gate -> Gate (Iterated Gate)
idHelp = Not . Free . Not

andG :: Iterated Gate -> Iterated Gate -> Iterated Gate
andG g1 g2 = Free $ And g1 g2

orG :: Iterated Gate -> Iterated Gate -> Iterated Gate
orG g1 g2 = Free $ Or g1 g2

notG :: Iterated Gate -> Iterated Gate
notG g1 = Free $ Not g1

var :: Iterated Gate
var = Pure ()
