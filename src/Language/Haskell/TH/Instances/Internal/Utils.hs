module Language.Haskell.TH.Instances.Internal.Utils
  (module Language.Haskell.TH.Instances.Internal.Utils
  ,module X) where
import Language.Haskell.TH as X
import Data.Map as X (Map, lookup,adjust,fromList,insert,traverseWithKey)

mapLookup :: Ord k => k -> Map k v -> Maybe v
mapLookup = X.lookup

type Set k = Map k ()

-- | Initialize a Map from a default value and a list of keys
fromKeys :: Ord k => v -> [k] -> Map k v
fromKeys z xs = let zs = z:zs in fromList $ zip xs zs

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

adjustMany :: (Ord k, Foldable t) => (a -> as -> as) -> Map k as -> t (k,a) -> Map k as
adjustMany ins m0 x = foldr (\(k,a) -> adjust (ins a) k) m0 x
