module Language.Haskell.TH.Instances.Internal.Defaults where
import Language.Haskell.TH.Instances.Internal.Utils
import Control.Concurrent.MVar (MVar,newMVar,modifyMVar,readMVar)
import GHC.Conc (pseq)
import Data.Map (Map)
import System.IO.Unsafe

defaults :: MVar (Map Name Exp)
{-# noinline defaults #-}
defaults = m `pseq` unsafePerformIO (newMVar m) where m = mempty

defaultMethod :: Name -> Q Exp -> Q [Dec]
defaultMethod n qe = do
  e <- qe
  runIO $ modifyMVar defaults (\m -> return (insert n e m,()))
  return []

getDefault :: Name -> Q (Maybe Dec)
getDefault n = runIO $ fmap exp_dec . mapLookup n <$> readMVar defaults
  where exp_dec e = ValD (VarP n) (NormalB e) []

