{-# LANGUAGE ScopedTypeVariables #-}

module MathStuff () where

type BFInput i = i -> Bool
type BF i = BFInput i -> Bool

multiCompose :: forall i j. BF i -> (i -> BF (j i)) -> BFInput (i, j i) -> Bool
multiCompose f gs x = f fInput
  where
    fInput :: BFInput i
    fInput i = g gInput
      where
        g :: BF (j i)
        g = gs i

        gInput :: BFInput (j i)
        gInput j = x (i, j)


setBit :: Eq i => i -> Bool -> BF i -> BF i
setBit i b f x = f fInput
  where
    fInput j
      | i == j = b
      | otherwise = x i
