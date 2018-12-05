{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}

module Prompt.Reflection where

import Aligned.Base
import Control.Arrow (Kleisli(..))
import Control.Category
import Control.Monad.State.Strict
import Control.Lens (makeClassy, (<<+=))
import Data.Default
import Data.Type.Coercion
import Prelude hiding (id,(.))
import qualified Prompt.Class as Class
import Prompt.Class (MonadPrompt)
import Ref.Key

data Prompt s r a = Prompt
  { _promptCokey :: {-# unpack #-} !(Cokey s a)
  , _promptId    :: {-# unpack #-} !Int
  }

instance TestCoercion (Prompt s r) where
  testCoercion (Prompt a i) (Prompt b j) = guard (i == j) *> testCoercion a b
 
newtype PromptEnv = PromptEnv { _freshPromptId :: Int }
  deriving Show

makeClassy ''PromptEnv 

instance Default PromptEnv where
  def = PromptEnv 0

type P = StateT PromptEnv

type K m = Rev Cat (Kleisli m)

-- TODO: fuse with CCT
data Action r m a where
  WithSub :: Prompt (KeyState m) r w -> (Sub r m a w -> CCT r m w) -> Action r m a
  P :: P m a -> Action r m a

data Delim r m a b
  = Delim {-# unpack #-} !(Prompt (KeyState m) r b) !(K (CCT r m) a b)

data Sub r m a b where
  Sub :: !(Rev Cat (Delim r m) a w) -> !(K (CCT r m) w b) -> Sub r m a b

data CCT r m a where
  CCT :: !(Action r m w) -> {-# unpack #-} !(Sub r m w a) -> CCT r m a 

instance a ~ b => Default (Sub r m a b) where
  def = Sub def def

singleSub :: (a -> CCT r m b) -> Sub r m a b
singleSub f = Sub def (singleton (Kleisli f))

instance Category (Sub r m) where
  id = Sub def def
  Sub cat_bw kwc . Sub cat_aw1 kw1b = case unsnoc cat_bw of
    Empty -> Sub cat_aw1 (kwc . kw1b)
    t :&: Delim p h -> Sub (snoc t (Delim p (h . kw1b)) . cat_aw1) kwc

(!>>=) :: CCT r m a -> Sub r m a b -> CCT r m b
CCT c r !>>= d = CCT c (d . r)

instance Monad m => Functor (CCT r m) where
  fmap = liftM

instance Monad m => Applicative (CCT r m) where
  pure a = CCT (P $ pure a) id
  (<*>) = ap

instance Monad m => Monad (CCT r m) where
  m >>= f = m !>>= singleSub f

instance MonadTrans (CCT r) where
  lift m = CCT (P (lift m)) id

instance MonadKey m => MonadPrompt (CCT r m) where
  type Prompt (CCT r m) = Prompt (KeyState m) r
  type Sub (CCT r m) = Sub r m 
  newPrompt = CCT (P $ Prompt <$> newCokey <*> (freshPromptId <<+= 1)) id
  pushPrompt p (CCT c (Sub d t)) = CCT c $ Sub (cons (Delim p t) d) id
  withSub p f = CCT (WithSub p f) id
  pushSub s (CCT c r) = CCT c (s . r)
