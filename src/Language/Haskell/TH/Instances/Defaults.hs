module Language.Haskell.TH.Instances.Defaults where
import Language.Haskell.TH.Instances.Defaults.Internal
import Language.Haskell.TH
import Control.Concurrent.MVar as X (MVar,newMVar,modifyMVar,readMVar)
import Data.Map (Map)
import qualified Data.Map as M
import System.IO.Unsafe

defaultMethod :: Name -> Name -> Q [Dec]
defaultMethod n s = unsafePerformIO (modifyMVar defaults (\m -> return (M.insert n s m,()))) `seq` return []

getDefault :: Name -> Q (Maybe Name)
getDefault n = runIO $ M.lookup n <$> readMVar defaults
