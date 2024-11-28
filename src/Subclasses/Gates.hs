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
  Gate,
  Gate',
  andG,
  orG,
  notG,
  var
) where
import           Complexity.BoFun             (BoFun (..), Constable (mkConst))
import           Control.DeepSeq              (NFData)
import           Data.Function.Memoize        (Memoizable, deriveMemoizable)
import           GHC.Generics                 (Generic)
import           Subclasses.Iterated.Iterated (Iterated, Iterated' (Iterated),
                                               iterId)
import           Test.QuickCheck              (Arbitrary, Gen, arbitrary,
                                               elements)
import           Utils                        (naturals)

data Gate = And | Or | Not | Id | Const Bool
  deriving (Eq, Ord, Show, Generic)

$(deriveMemoizable ''Gate)

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

newtype Gate' = Gate' Gate
  deriving (Memoizable, NFData)

instance BoFun Gate' Int where
  isConst :: Gate' -> Maybe Bool
  isConst (Gate' f) = isConst f
  variables :: Gate' -> [Int]
  variables (Gate' f) = take (arityGate f) naturals
  setBit :: (Int, Bool) -> Gate' -> Gate'
  setBit (_, v) (Gate' f) = Gate' $ setBit ((), v) f

-------------- Iterated gates -------------------------

gateHelper :: Gate -> [Iterated Gate'] -> Iterated Gate'
gateHelper g = Iterated (Gate' g)

andG :: Iterated Gate' -> Iterated Gate' -> Iterated Gate'
andG g1 g2 = gateHelper And [g1, g2]

orG :: Iterated Gate' -> Iterated Gate' -> Iterated Gate'
orG g1 g2 = gateHelper Or [g1, g2]

notG :: Iterated Gate' -> Iterated Gate'
notG g1 = gateHelper Not [g1]

var :: Iterated Gate'
var = iterId
