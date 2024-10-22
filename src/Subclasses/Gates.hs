{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE TemplateHaskell       #-}
module Subclasses.Gates (
  Gate,
  andG,
  orG,
  notG,
  var
) where
import           BoFun                 (BoFun (..), Constable (mkConst))
import           Data.Function.Memoize (deriveMemoizable)
import qualified Data.MultiSet         as MultiSet
import           Subclasses.Iterated   (IteratedSymm, idIter, liftIter)
import           Subclasses.Lifted     (liftFunSymm)

data Gate = And | Or | Not | Id | Const Bool
  deriving (Eq, Ord, Show)

$(deriveMemoizable ''Gate)

instance BoFun Gate () where
  isConst :: Gate -> Maybe Bool
  isConst g = case g of
    Const val -> Just val
    _         -> Nothing
  variables :: Gate -> [()]
  variables g = case g of
    Const _ -> []
    _       -> [()]
  setBit :: ((), Bool) -> Gate -> Gate
  setBit (_, val) g = case g of
    And     -> if val then Id else Const False
    Or      -> if val then Const True else Id
    Not     -> Const $ not val
    Id      -> Const val
    Const _ -> undefined

instance Constable Gate where
  mkConst :: Bool -> Gate
  mkConst = Const

-------------- Iterated gates -------------------------

gateHelper :: Gate -> [IteratedSymm Gate] -> IteratedSymm Gate
gateHelper g subGates = liftIter $ liftFunSymm g $ MultiSet.fromList subGates

andG :: IteratedSymm Gate -> IteratedSymm Gate -> IteratedSymm Gate
andG g1 g2 = gateHelper And [g1, g2]

orG :: IteratedSymm Gate -> IteratedSymm Gate -> IteratedSymm Gate
orG g1 g2 = gateHelper Or [g1, g2]

notG :: IteratedSymm Gate -> IteratedSymm Gate
notG g1 = gateHelper Not [g1]

var :: IteratedSymm Gate
var = idIter
