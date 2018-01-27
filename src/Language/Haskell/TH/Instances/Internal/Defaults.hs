{-# language MagicHash #-}
module Language.Haskell.TH.Instances.Internal.Defaults (defaults#,getDefault, module X) where
import Language.Haskell.TH.Instances.Internal.Utils as X
import Control.Concurrent.MVar (MVar,newMVar,readMVar)
import GHC.Conc (pseq)
import System.IO.Unsafe

-- | Global map from method names to definitions.
-- Do not use directly, preferring 'defaulting' and 'getDefault'
defaults# :: MVar (Map Name Exp)
{-# noinline defaults# #-}
defaults# = m `pseq` unsafePerformIO (newMVar m) where m = mempty

-- | Generate a method definition for provided 'Name' if a default exists in scope
getDefault :: Name -> Q (Maybe Dec)
getDefault n = runIO $ fmap exp_dec . mapLookup n <$> readMVar defaults#
  where exp_dec e = ValD (VarP n) (NormalB e) []

