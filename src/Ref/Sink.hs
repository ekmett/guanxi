{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Ref.Sink where

import Control.Monad.State.Class
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void
import Ref.Signal

data Sink m a = Sink 
  { _cellIds    :: !(Signals m)
  , _cellUpdate :: a -> m () -- update
  }

instance Contravariant (Sink m) where
  contramap f (Sink m g) = Sink m (g . f)
 
instance Applicative m => Divisible (Sink m) where
  conquer = Sink mempty $ \_ -> pure ()
  divide f (Sink s g) (Sink t h) = Sink (s <> t) $ \a -> case f a of 
     (b, c) -> g b *> h c

instance Applicative m => Decidable (Sink m) where
  lose f = Sink mempty (absurd . f)
  choose f (Sink s g) (Sink t h) = Sink (s <> t) $ \a -> case f a of
     Left b -> g b
     Right c -> h c

instance HasSignals m (Sink m a) where
  signals (Sink s _) = s

writeSink :: (MonadState s m, HasSignalEnv s m) => Sink m a -> a -> m ()
writeSink (Sink _ u) a = scope $ u a
