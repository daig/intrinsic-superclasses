{-# language ScopedTypeVariables #-}
{-# language LambdaCase #-}
{-# language ViewPatterns #-}
module Language.Haskell.TH.Instances (instances) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Haskell.Meta.Parse (parseDecs)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))

-- | @QuasiQuoter@ for providing <https://ghc.haskell.org/trac/ghc/wiki/IntrinsicSuperclasses intrinsic-superclasses>.
--
-- Example:
--
-- >  class Semigroup a where mappend :: a -> a -> a
-- >  class Semigroup a => Monoid a where mempty :: a
-- >  class (Monoid a) => Group a where inverse :: a -> a
-- >  [instances| Num a => Group a where
-- >      mempty = fromInteger 0
-- >      mappend a b = a + b
-- >      inverse = negate
-- >      |]
--
-- will generate the appropriate instances for @Semigroup@, @Monoid@, and @Group@:
--
-- >  instance Num a => Semigroup a where mappend a b = a + b
-- >  instance Num a => Monoid a where mempty = fromInteger 0
-- >  instance Num a => Group a where inverse = negate
instances :: QuasiQuoter
instances = QuasiQuoter
  {quoteExp = err "Exp"
  ,quotePat = err "Pat"
  ,quoteType = err "Type"
  ,quoteDec = \s -> case parseDecs ("instance " ++ s) of
    Left e -> error e
    Right d -> fmap concat $ mapM splitInstances d}
  where err s = const $ error $ "quasiquoter `instances` expected Dec, instead used as " ++ s
  

-- | Implements the @instances@ quasiquoter ast tranform
splitInstances :: Dec -> DecsQ
splitInstances = \case
  InstanceD Nothing ctx (AppT _ instancesFor) instanceMethods -> do
    let instanceMethods' = M.fromList [(defName d,d) | d <- instanceMethods]
    classOps <- getClassOps instanceMethods 
    let classDefs = M.map (\(S.map (occName . snd) -> names) -> (M.mapKeys occName instanceMethods' M.!) `S.map` names) classOps
    pure $ M.foldrWithKey (\c ms -> (declInstance ctx c instancesFor ms :)) [] classDefs
  d -> error $ "splitInstances: Not an instance declaration\n" ++ pprint d

-- | helper constructor for declaring an instance
declInstance :: Cxt -> Name -> Type -> Set Dec -> Dec
declInstance ctx className targetType ms = InstanceD Nothing ctx (AppT (ConT className) targetType) (S.toList ms)
    
-- | Create a Map of className to method declaration from a list of instance method definitions
getClassOps :: Traversable t => t Dec -> Q (Map ParentName (Set (Type,Name)))
getClassOps decs = collectFromList S.singleton <$> mapM (\d -> opClass <$> reify (defName d)) decs
  where
    opClass (ClassOpI n t p) = (p,(t,n))
    opClass x = error $ "opClass: not a class operation\n" ++ pprint x

-- | Get the name of a function or value declaration
defName :: Dec -> Name
defName x = case x of
  FunD n _ -> n
  ValD (VarP n) _ _ -> n
  d -> error $ "defName: Declaration is not a Function or Value definition\n" ++ pprint d

-- | Get the OccName out of a Name
occName :: Name -> String
occName (Name (OccName s) _) = s

-- | Create a map from a Foldable collection, monoidally combining values with the same key
collectFromList :: (Ord k,Monoid m, Foldable t) => (v -> m) -> t (k,v) -> Map k m
collectFromList f = foldr (\(k,v) -> M.insertWith (<>) k $ f v) M.empty
