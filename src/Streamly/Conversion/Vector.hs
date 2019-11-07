module Streamly.Conversion.Vector where

import Streamly

import Data.Vector (Vector)
import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Unfold.Types (Unfold(..))
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))

import qualified Data.Vector as V
import qualified Streamly.Prelude as S

import Prelude hiding (read, write)

read :: Monad m => Unfold m (Vector a) a
read = Unfold step inject
  where
    inject = return
    step v
      | V.null v = return Stop
      | otherwise = return $ Yield (V.unsafeHead v) (V.unsafeTail v)

write :: Monad m => Fold m a (Vector a)
write = Fold step initial done
  where
    step x a = return $ V.snoc x a
    initial = return V.empty
    done = return




