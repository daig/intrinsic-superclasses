{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language FlexibleContexts #-}
{-# language QuasiQuotes #-}
{-# language GADTs #-}
module Language.Haskell.TH.Instances (instances) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Haskell.Meta.Parse (parseDecs)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Data.Set as S
import Data.Set (Set)
import Data.List (partition)
import Control.Monad.Writer (when,lift,execWriterT,Endo(..),MonadWriter(..))

instances :: QuasiQuoter
instances = QuasiQuoter
  {quoteExp = err "Exp"
  ,quotePat = err "Pat"
  ,quoteType = err "Type"
  ,quoteDec = \s -> case parseDecs ("instance " ++ s) of {Left e -> error e; Right d -> fmap concat $ mapM splitInstances d}}
  where err s = const $ error $ "quasiquoter `instances` expected Dec, instead used as " ++ s
  
tellCons :: MonadWriter (Endo [a]) m => a -> m ()
tellCons = tell . Endo . (:)

splitInstances :: Dec -> DecsQ
splitInstances = \case
  InstanceD _overlaps ctx ty@(AppT _ (ConT instanceFor)) instanceMethods ->
    let
      go methods  = \case
        (AppT (ConT className) _) -> do
          (superclasses,classMethods) <- lift $ reifyClass className 
          let (theseMethods,methods') = partition (\(FunD (Name occ _) _) -> S.member occ (S.map (\(Name o _) -> o) classMethods)) methods
          when (length theseMethods > 0) $ tellCons $ InstanceD Nothing ctx (AppT (ConT className) (ConT instanceFor)) theseMethods
          mapM_ (go methods') superclasses
        {-x -> error $ "splitInstances: malformed instance head (" ++ show x ++ ")"-}
        _ -> pure ()
    in (`appEndo` []) <$> execWriterT (go instanceMethods ty)
  _ -> error "reifyInstance: not an instance declaration"

reifyClass :: Name -> Q (Cxt,Set Name)
reifyClass n = do
  info <- reify n
  pure $ case info of
    ClassI (ClassD ctx _name  _tyvarbndr _fundeps methods) _instances -> (ctx,S.fromList [m | SigD m _ <- methods])
    _ -> error "reifyClass: not a class name"
