module Modeling.Util where

import           Control.Lens         (Lens', over, view)
import           Control.Monad.Reader (MonadReader, local)
import           Data.Foldable        (traverse_)
import           Data.Map             (Map)
import qualified Data.Map             as Map

localMod :: MonadReader x m => Lens' x y -> (y -> y) -> m z -> m z
localMod lens mod = local (over lens mod)

insertAll :: (Foldable t, Ord a) => t (a, b) -> Map a b -> Map a b
insertAll nxs m0 = foldl (\m (n, x) -> Map.insert n x m) m0 nxs

mergeMaps :: Ord a => Map a b -> Map a b -> Map a b
mergeMaps m1 m0 = insertAll (Map.toList m1) m0

maybeLocal :: (a -> m b -> m b) -> (Maybe a -> m b -> m b)
maybeLocal f = maybe id f

traverseMap_ :: Applicative m => (k -> v -> m b) -> Map k v -> m ()
traverseMap_ f = traverse_ (uncurry f) . Map.toList

readerLookup :: (MonadReader x m, Ord y) => Lens' x (Map y z) -> y -> m (Maybe z)
readerLookup lens y  = Map.lookup y <$> view lens

readerGet :: (MonadReader x m, Ord y) => Lens' x (Map y z) -> (y -> m z) -> y -> m z
readerGet lens alt y = readerLookup lens y >>= maybe (alt y) pure
