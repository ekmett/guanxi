{-# language PatternSynonyms #-}
{-# language ImplicitParams #-}
{-# language ConstraintKinds #-}
{-# language ViewPatterns #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language UnboxedTuples #-}
module SAT where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Primitive.Types
import Data.Word
import Vec

newtype Val = Val Word8
  deriving (Eq,Ord,Prim)

pattern FALSE, TRUE, UNKNOWN :: Val
pattern FALSE = Val 0
pattern TRUE = Val 1
pattern UNKNOWN = Val 2

known :: Val -> Maybe Bool
known UNKNOWN = Nothing
known FALSE = Just False
known TRUE = Just True

pattern Known :: Bool -> Val
pattern Known v <- (known -> Just v) where
  Known b = Val (fromIntegral $ fromEnum b)

{-# complete TRUE, FALSE, UNKNOWN #-}
{-# complete Known, UNKNOWN #-}

instance Show Val where
  show TRUE = "TRUE"
  show FALSE = "FALSE"
  show UNKNOWN = "UNKNOWN"

newtype Lit s = LIT Int
  deriving (Eq,Ord,Prim)

mklit :: Var s -> Bool -> Lit s
mklit (Var i) False = LIT i
mklit (Var i) True = LIT (complement i)

decode :: Lit s -> (Var s, Bool)
decode (LIT i)
  | i < 0 = (Var (complement i), True)
  | otherwise = (Var i, False)

pattern Lit :: Var s -> Bool -> Lit s
pattern Lit i s <- (decode -> (i, s)) where
  Lit i s = mklit i s

data SAT s = SAT
  { _assignments :: Vector s Val
  , _trail :: Vector s (Lit s)
  }

assignments :: GivenSAT s => Vector s Val
assignments = _assignments ?sat

trail :: GivenSAT s => Vector s (Lit s)
trail = _trail ?sat

type GivenSAT s = (?sat :: SAT s)

newtype Var s = Var Int

type MonadSAT m = (PrimMonad m, MonadPlus m, GivenSAT (PrimState m))
type ReadMonadSAT m = (PrimMonad m, GivenSAT (PrimState m))

newVar :: MonadSAT m => m (Var (PrimState m))
newVar = Var <$> addBackVector UNKNOWN assignments
{-# inline newVar #-}

readVar :: ReadMonadSAT m => Var (PrimState m) -> m Val
readVar (Var i) = readVector assignments i
{-# inline readVar #-}

-- blow up the universe on contradiction here
writeVar :: MonadSAT m => Var (PrimState m) -> Bool -> Bool -> m ()
writeVar v@(Var i) b entrail = do
  let new = Known b
  old <- writeBackVector assignments i new
  guard (old == UNKNOWN || old == new)
  when (old == UNKNOWN && entrail) $
    () <$ addBackVector (Lit v b) trail
{-# inline writeVar #-}
