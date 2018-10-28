module Dyn where

data Dyn a
  = D0
  | D1 !a
  | D2 !a !a a !(Dyn a)
  | D3 !a !a !a a !(Dyn a)

cons :: Monoid a => a -> Dyn a -> Dyn a
cons a D0 = D1 a
cons a (D1 b) = D2 a b (a <> b) D0
cons a (D2 b c bc rest) = D3 a b c bc rest
cons a (D3 b _ _ !cd rest) = D2 a b (a <> b) (cons cd rest)

query :: Monoid b => (a -> b) -> Dyn a -> b
query _ D0 = mempty
query f (D1 a) = f a
query f (D2 a b _ rest) = f a <> f b <> query f rest
query f (D3 a b c _ rest) = f a <> f b <> f c <> query f rest
