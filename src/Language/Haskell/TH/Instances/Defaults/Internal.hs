module Language.Haskell.TH.Instances.Defaults.Internal where
import Language.Haskell.TH
import Control.Concurrent.MVar as X (MVar,newMVar,modifyMVar,readMVar)
import Data.Map (Map)
import qualified Data.Map as M
import System.IO.Unsafe
import GHC.Conc (pseq)

defaults :: MVar (Map Name Exp)
{-# noinline defaults #-}
defaults = m `pseq` unsafePerformIO (newMVar m) where m = M.empty
