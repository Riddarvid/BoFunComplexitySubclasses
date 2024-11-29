{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Subclasses.Gates (
  Gate(And, Or, Not),
  NonSymmGate,
  iterAnd,
  iterOr,
  iterNot
) where
import           Complexity.BoFun             (BoFun (..), Constable (mkConst))
import           Control.DeepSeq              (NFData)
import           Data.Function.Memoize        (Memoizable, deriveMemoizable)
import           Exploration.PrettyPrinting   (PrettyBoFun (prettyShow))
import           GHC.Generics                 (Generic)
import           Subclasses.Iterated.Iterated (Iterated, Iterated' (Iterated))
import           Test.QuickCheck              (Arbitrary, Gen, arbitrary,
                                               elements)
import           Utils                        (naturals)

data Gate = And | Or | Not | Id | Const Bool
  deriving (Eq, Ord, Show, Generic)

$(deriveMemoizable ''Gate)

instance PrettyBoFun Gate where
  prettyShow :: Gate -> String
  prettyShow And           = "∧"
  prettyShow Or            = "∨"
  prettyShow Not           = "¬"
  prettyShow Id            = "x"
  prettyShow (Const False) = "F"
  prettyShow (Const True)  = "T"

instance NFData Gate

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

instance Arbitrary Gate where
  arbitrary :: Gen Gate
  arbitrary = elements [And, Or, Not, Id, Const False, Const True]

arityGate :: Gate -> Int
arityGate And       = 2
arityGate Or        = 2
arityGate Not       = 1
arityGate Id        = 1
arityGate (Const _) = 0

newtype NonSymmGate = NonSymmGate Gate
  deriving (Memoizable, NFData)

instance PrettyBoFun NonSymmGate where
  prettyShow :: NonSymmGate -> String
  prettyShow (NonSymmGate f) = prettyShow f

instance BoFun NonSymmGate Int where
  isConst :: NonSymmGate -> Maybe Bool
  isConst (NonSymmGate f) = isConst f
  variables :: NonSymmGate -> [Int]
  variables (NonSymmGate f) = take (arityGate f) naturals
  setBit :: (Int, Bool) -> NonSymmGate -> NonSymmGate
  setBit (_, v) (NonSymmGate f) = NonSymmGate $ setBit ((), v) f

-------------- Iterated gates -------------------------

gateHelper :: Gate -> [Iterated NonSymmGate] -> Iterated NonSymmGate
gateHelper g = Iterated (NonSymmGate g)

iterAnd :: Iterated NonSymmGate -> Iterated NonSymmGate -> Iterated NonSymmGate
iterAnd g1 g2 = gateHelper And [g1, g2]

iterOr :: Iterated NonSymmGate -> Iterated NonSymmGate -> Iterated NonSymmGate
iterOr g1 g2 = gateHelper Or [g1, g2]

iterNot :: Iterated NonSymmGate -> Iterated NonSymmGate
iterNot g1 = gateHelper Not [g1]
