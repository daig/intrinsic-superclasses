module Language.Haskell.TH.Instances (instances) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Haskell.Meta.Parse (parseDecs)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (partition)
import Control.Monad.Writer (when,lift,execWriterT,Endo(..),MonadWriter(..))

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
  

splitInstances :: Dec -> DecsQ
splitInstances d = case d of
  InstanceD _overlaps ctx ty@(AppT _ instanceFor) instanceMethods ->
    let
      go methods t = case t of
        AppT (ConT className) _ -> do
          (superclasses,classMethods) <- lift $ reifyClass className 
          let (theseMethods,methods') = partition (\x -> defOccName x `S.member` classMethods) methods
          when (length theseMethods > 0) $ tellCons $ InstanceD Nothing ctx (AppT (ConT className) instanceFor) theseMethods
          mapM_ (go methods') superclasses
        _ -> error $ "splitInstances: malformed instance head (" ++ show t ++ ")"
        {-_ -> pure ()-}
    in (`appEndo` []) <$> execWriterT (go instanceMethods ty)
  _ -> error $ "splitInstances: not an instance declaration " ++ show d
  where
    tellCons = tell . Endo . (:)
    defOccName x = case x of
      FunD (Name occ _) _ -> occ
      ValD (VarP (Name occ _)) _ _ -> occ
      _ -> error $ "defOccName: not a function or value definition " ++ show x
    reifyClass :: Name -> Q (Cxt,Set OccName)
    reifyClass n = do
      info <- reify n
      pure $ case info of
        ClassI (ClassD ctx _name  _tyvarbndr _fundeps methods) _instances -> (ctx,S.fromList [occ | SigD (Name occ _) _ <- methods])
        _ -> error "reifyClass: not a class name"
