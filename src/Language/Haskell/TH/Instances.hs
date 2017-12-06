{-# language ScopedTypeVariables #-}
{-# language TupleSections #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
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
import Data.Maybe (mapMaybe)
import Control.Monad.Writer
import Data.Foldable

-- | @QuasiQuoter@ for providing <https://ghc.haskell.org/trac/ghc/wiki/IntrinsicSuperclasses intrinsic-superclasses>.
--
-- Example:
--
-- >  class Semigroup a where mappend :: a -> a -> a
-- >  class Semigroup a => Commutative a
-- >  class Semigroup a => Monoid a where mempty :: a
-- >  class Monoid a => Group a where inverse :: a -> a
-- >  class (Commutative a, Group a) => CommutativeGroup a
-- >  [instances| Num a => CommutativeGroup a where
-- >      mempty = fromInteger 0
-- >      mappend a b = a + b
-- >      inverse = negate
-- >      |]
--
-- will generate the appropriate instances for @Semigroup@, @Monoid@, and @Group@:
--
-- >  instance Num a => Semigroup a where mappend a b = a + b
-- >  instance Num a => Commutative a
-- >  instance Num a => Monoid a where mempty = fromInteger 0
-- >  instance Num a => Group a where inverse = negate
-- >  instance Num a => CommutativeGroup a
instances :: QuasiQuoter
instances = QuasiQuoter
  {quoteExp = err "Exp"
  ,quotePat = err "Pat"
  ,quoteType = err "Type"
  ,quoteDec = \s -> case parseDecs ("instance " ++ s) of
    Left e -> error e
    Right d -> fmap concat $ mapM splitInstances d}
  where err s = const $ error $ "quasiquoter `instances` expected Dec, instead used as " ++ s

-- | Implements the @instances@ quasiquoter ast transform
splitInstances :: Dec -> DecsQ
splitInstances = \case
  InstanceD Nothing ctx (AppT (ConT className) instancesFor) instanceMethods -> do
    instanceMethods' <- M.fromList <$> traverse globalizeDef instanceMethods
    superclasses <- getTransitiveSuperclassNames className
    superclassHasInstance <- M.traverseWithKey (\k _ -> isInstance k [instancesFor]) superclasses
    let superclasses' = M.filterWithKey (\k _ -> not $ superclassHasInstance M.! k) superclasses
    classOps <- getClassOps instanceMethods superclasses'
    let classDefs = M.map (\names -> (instanceMethods' M.!) `S.map` names) classOps
    pure $ M.foldrWithKey (\c ms -> (declInstance ctx c instancesFor ms :)) [] classDefs
  d -> error $ "splitInstances: Not an instance declaration\n" ++ pprint d
  where
    occName (Name (OccName s) _) = s
    declInstance ctx className targetType ms = InstanceD Nothing ctx (AppT (ConT className) targetType) (S.toList ms)
    -- Associate a definition with its toplevel qualified identifier
    globalizeDef d = (lookupValueName . occName . defName) d >>= \case
        Nothing -> error $ "globalizeDef: instance method " ++ show (occName (defName d)) ++ " not in scope"
        Just n -> pure (n,d)
    
-- | Create a Map of className to method declaration from a list of instance method definitions
getClassOps :: Traversable t => t Dec -> Map ParentName (Set Name) -> Q (Map ParentName (Set Name))
getClassOps decs superclasses = collectFromList S.insert superclasses <$> mapM (\d -> opClass <$> reify (defName d)) decs
  where
    opClass (ClassOpI n _t p) = (p,n)
    opClass x = error $ "opClass: not a class operation\n" ++ pprint x

-- | Get the name of a function or value declaration
defName :: Dec -> Name
defName x = case x of
  FunD n _ -> n
  ValD (VarP n) _ _ -> n
  d -> error $ "defName: Declaration is not a Function or Value definition\n" ++ pprint d


collectFromList :: (Ord k, Foldable t) => (a -> as -> as) -> Map k as -> t (k,a) -> Map k as
collectFromList f m0 x = foldr (\(k,a) -> M.adjust (f a) k) m0 x

-- | reify the names of the direct superclasses for a class name
getSuperclassNames :: Name -> Q [Name]
getSuperclassNames className = do
  ClassI (ClassD ctx _  (S.fromList . map _TyVarBndr_name -> classVars) _ _) _ <- reify className
  let
    -- if t represents a supeclass of n then `superclass t` is Just the superclass name, and Nothing otherwise
    superclass :: Type -> Maybe Name
    superclass = \case
      AppT t (VarT v) | S.member v classVars -> Just $ headAppT t
      AppT ConT{} _ -> Nothing
      AppT t _ -> superclass t
      x -> error $ show x
  pure $ mapMaybe superclass ctx
  where
    _TyVarBndr_name = \case {PlainTV n -> n; KindedTV n _ -> n}
    headAppT :: Type -> Name -- project the innermost @ConT@ in a chain of @AppT@
    headAppT = \case
      ConT n -> n
      AppT t _ -> headAppT t
      x -> error $ "headAppT: Malformed type\n" ++ show x

-- | reify the names of all transitive superclasses for a class name, including itself
getTransitiveSuperclassNames :: Name -> Q (Map Name (Set Name))
getTransitiveSuperclassNames = execWriterT . go where
  go n = do
    tell $ M.singleton n S.empty
    traverse_ go =<< lift (getSuperclassNames n)
