module Language.Haskell.TH.Instances.Defaults where
import Language.Haskell.TH.Instances.Defaults.Internal
import Language.Haskell.TH
import Control.Concurrent.MVar as X (MVar,newMVar,modifyMVar,readMVar)
import Data.Map (Map)
import qualified Data.Map as M
import System.IO.Unsafe

defaultMethod :: Name -> Q Exp -> Q [Dec]
defaultMethod n qe = do
  e <- qe
  runIO $ modifyMVar defaults (\m -> return (M.insert n e m,()))
  return []

getDefault :: Name -> Q (Maybe Dec)
getDefault n = runIO $ fmap exp_dec . M.lookup n <$> readMVar defaults
  where exp_dec e = ValD (VarP n) (NormalB e) []

