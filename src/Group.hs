module Group where

import Data.Monoid

class Monoid a => Group a where
  inv :: a -> a

instance Group () where
  inv _ = ()

instance Num a => Group (Sum a) where
  inv (Sum a) = Sum (negate a)

instance (Group a, Group b) => Group (a, b) where
  inv (a, b) = (inv a, inv b)
