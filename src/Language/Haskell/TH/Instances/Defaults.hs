{-# language MagicHash #-}
module Language.Haskell.TH.Instances.Defaults where
import Language.Haskell.TH.Instances.Internal.Defaults
import Control.Concurrent.MVar (modifyMVar)

-- | Give a default for a typeclass method that will be utilized by the @instances@ quasiquoter.
-- The default will be implicitly brought into scope when the module is imported, like typeclass instances.
--
-- Example:
--
-- > module Data.Traversable.Defaults (module X) where
-- > import Data.Traversable as X
-- > import Data.Functor.Identity as X
-- > import Data.Functor.Const as X
-- > defaultMethod 'fmap [|\f -> runIdentity . traverse (Identity . f)|]
-- > defaultMethod 'foldmap [|\f -> getConst . traverse (Const . f)|]
--
-- > module MyData where
-- > import Data.Traversable.Defaults
-- >
-- > data Foo a = Foo a a
-- > [instances| Travesable Foo where traverse f (Foo a a') = Foo <$> f a <*> f a'|]
--
-- will generate
--
-- > instance Functor Foo where fmap = \f -> runIdentity . traverse (Identity . f)
-- > instance Foldable Foo where foldMap = \f -> getConst . traverse (Const . f)
-- > instance Travesable Foo where traverse f (Foo a a') = Foo <$> f a <*> f a'
defaulting :: Name -> Q Exp -> Q [Dec]
defaulting n qe = do
  e <- qe
  runIO $ modifyMVar defaults# (\m -> return (insert n e m,()))
  return []
