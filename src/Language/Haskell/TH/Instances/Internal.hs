{-# language ScopedTypeVariables #-}
{-# language TupleSections #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language ViewPatterns #-}
module Language.Haskell.TH.Instances.Internal
  (module Language.Haskell.TH.Instances.Internal
  ,module X) where

import Language.Haskell.TH as X
import Language.Haskell.TH.Instances.Defaults as X
import Language.Haskell.TH.Syntax as X hiding (lift)
import Language.Haskell.Meta.Parse as X (parseDecs)
import Language.Haskell.TH.Quote as X (QuasiQuoter(..))
import Data.Map as X (Map)
import qualified Data.Map as M
import Data.Maybe as X (mapMaybe)
import Control.Monad.Writer as X
import Data.Foldable as X

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
  InstanceD Nothing ctx (AppT (ConT className) instancesFor) declaredMethods -> do
    declaredMethods' <- M.fromList <$> traverse globalizeDef declaredMethods
    superclasses <- getTransitiveSuperclassNames className

    requiredMethods <- fold <$> M.traverseWithKey (\k _ -> getClassMethods k) superclasses
    let badMethods = filter (\x -> not $ M.member x requiredMethods) $ M.keys declaredMethods'
    unless (null badMethods) $
      error $ "splitInstances: Trying to declare methods not in the superclass heirarchy\n"
           ++ unlines (map show badMethods)

    defaultMethods <- M.traverseMaybeWithKey (\k _ -> getDefault k)  requiredMethods
    let declaredMethods'' = declaredMethods' `M.union` defaultMethods

    superclassHasInstance <- M.traverseWithKey (\k _ -> isInstance k [instancesFor]) superclasses
    superclasses' <- fmap (fromKeys M.empty) $ traverse globalizeClass $ filter (\k -> not $ superclassHasInstance M.! k) $ M.keys superclasses

    classOps <- getClassOps (M.elems declaredMethods'') superclasses'

    let classDefs = M.map (\names -> (declaredMethods'' M.!) `M.mapKeys` names) classOps
    let instanceDecls = M.foldrWithKey (\c ms -> (declInstance ctx c instancesFor ms :)) [] classDefs
    pure instanceDecls
  d -> error $ "splitInstances: Not an instance declaration\n" ++ pprint d
  where
    declInstance ctx className targetType ms = InstanceD Nothing ctx (AppT (ConT className) targetType) (M.keys ms)
    -- Associate a definition with its toplevel qualified identifier
    globalizeDef d = (lookupValueName . occName . defName) d >>= \case
        Nothing -> error $ "globalizeDef: instance method " ++ show (occName (defName d)) ++ " not in scope"
        Just n -> pure (n,d)

-- | Get the fully qualified name of a class
globalizeClass :: Name -> Q Name
globalizeClass c = (lookupTypeName . occName) c >>= \case
    Nothing -> error $ "globalizeClass: class " ++ show (occName c) ++ " not in scope"
    Just n -> pure n
    
-- | Create a Map of className to method declaration from a list of instance method definitions
getClassOps :: Traversable t => t Dec -> Map ParentName (Set Name) -> Q (Map ParentName (Set Name))
getClassOps decs superclasses = collectFromList (`M.insert` ()) superclasses <$> mapM (\d -> opClass <$> reify (defName d)) decs
  where
    opClass :: Info -> (ParentName, Name)
    opClass (ClassOpI n _t p) = (p,n)
    opClass x = error $ "opClass: not a class operation\n" ++ pprint x


-- | Get the name of a function or value declaration
defName :: Dec -> Name
defName x = case x of
  FunD n _ -> n
  ValD (VarP n) _ _ -> n
  d -> error $ "defName: Declaration is not a Function or Value definition\n" ++ pprint d
sigName :: Dec -> Name
sigName = \case
  SigD n _ -> n
  d -> error $ "sigName: Declaration is not a type signature\n" ++ pprint d


collectFromList :: (Ord k, Foldable t) => (a -> as -> as) -> Map k as -> t (k,a) -> Map k as
collectFromList f m0 x = foldr (\(k,a) -> M.adjust (f a) k) m0 x

-- | reify the names of the direct superclasses for a class name
getSuperclassNames :: Name -> Q [Name]
getSuperclassNames className = do
  ClassI (ClassD ctx _  (fromKeys () . map _TyVarBndr_name -> classVars) _ _) _ <- reify className
  let
    -- if t represents a supeclass of n then `superclass t` is Just the superclass name, and Nothing otherwise
    superclass :: Type -> Maybe Name
    superclass = \case
      AppT t (VarT v) | M.member v classVars -> Just $ headAppT t
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

getClassMethods :: Name -> Q (Set Name)
getClassMethods className = reify className <&> (\(ClassI (ClassD _ _ _ _ (map sigName -> methods)) _) -> fromKeys () methods)

type Set k = Map k ()
fromKeys :: Ord k => v -> [k] -> Map k v
fromKeys z xs = let zs = z:zs in M.fromList $ zip xs zs
-- | reify the names of all transitive superclasses for a class name, including itself
getTransitiveSuperclassNames :: Name -> Q (Map Name (Set a))
getTransitiveSuperclassNames = execWriterT . go where
  go n = do
    tell $ M.singleton n M.empty
    traverse_ go =<< lift (getSuperclassNames n)

-- | Extract the unqualified part from a @Name@. For example:
--
-- > show ''Show === "GHC.Show.Show"
-- > occName ''Show === "Show"
occName :: Name -> String
occName (Name (OccName s) _) = s

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
