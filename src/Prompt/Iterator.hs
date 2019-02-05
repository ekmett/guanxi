{-# language LambdaCase #-}
module Prompt.Iterator where

import Data.Foldable
import Data.Semigroup
import Prompt.Class
import Unaligned.Base

data Iterator m a = Iterator a (m (Iterator m a)) | Done

next :: Applicative m => Iterator m a -> View a (m (Iterator m a))
next (Iterator a m) = a :&: m
next Done = Empty

iterator :: MonadPrompt m => ((a -> m ()) -> m ()) -> m (Iterator m a)
iterator loop = reset $ \p -> Done <$ loop (\a -> shift p $ \k -> return $ Iterator a $ k ())
