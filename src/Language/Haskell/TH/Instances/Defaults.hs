{-# language DeriveDataTypeable #-}
module Language.Haskell.TH.Instances.Defaults where
import Data.Data
import Language.Haskell.TH

-- | Give a default for a typeclass method that will be utilized by the 'instances' quasiquoter.
-- Defaults are declared by giving an [annotation](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#annotating-types) like:
--
-- > {-# ann type MySubClass (Defaults 'mySuperclassMethod 'myDefaultDefinition) #-}
--
-- For example, we could modify "Data.Traversable" to work with 'instances' like so:
--
-- > {-# language TemplateHaskell #-}
-- > module Data.Traversable where
-- > {- ... normal imports ... -}
-- > import Language.Haskell.TH.Instances.Defaults
-- >
-- > class (Functor t, Foldable t) => Traversable t where ...  -- Same as normal
-- > {-# ANN type Traversable (Defaults 'fmap 'fmapDefault) #-}
-- > {-# ANN type Traversable (Defaults 'foldMap 'foldMapDefault) #-}
--
-- > module MyData where
-- > import Data.Traversable
-- >
-- > data Foo a = Foo a a
-- > [instances| Travesable Foo where traverse f (Foo a a') = Foo <$> f a <*> f a'|]
--
-- will generate
--
-- > instance Functor Foo where fmap = fmapDefault
-- > instance Foldable Foo where foldMap = foldMapDefault
-- > instance Travesable Foo where traverse f (Foo a a') = Foo <$> f a <*> f a'
data Defaults = Defaults
  {defining :: Name -- ^ The name of the superclass method being provided
  ,definition :: Name -- ^ The name of a function implementing the superclass method
  }
  deriving (Data,Typeable,Show)
