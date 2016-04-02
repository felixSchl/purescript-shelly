module Shelly where

import Prelude
import Debug.Trace
import Control.Monad (when, unless)
import Control.Monad.Eff (Eff())
import Control.Bind ((>=>))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State.Trans (StateT(), evalStateT, modify, get)
import Control.Monad.State.Trans as State
import Control.Monad.Trans (lift)
import Node.Path as Path
import Node.Path (FilePath)
import Node.Process as Process
import Node.Process (PROCESS)
import Node.FS (FS)
import Node.FS as FS
import Node.FS.Sync as FS
import Node.FS.Stats as FS
import Unsafe.Coerce
import Data.StrMap (StrMap())
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error)

type Env = StrMap String
type State = { cwd :: String, env :: Env }
type Sh e a = StateT State (Eff e) a

runSh :: forall a e. Sh e a -> State -> Eff e a
runSh = evalStateT

shelly :: forall e a
        . Sh (process :: PROCESS | e) a
       -> Eff (process :: PROCESS | e) a
shelly action = do
  cwd <- Process.cwd
  env <- Process.getEnv
  runSh action { cwd: cwd, env: env }

resolvePath :: forall e. FilePath -> Sh e FilePath
resolvePath p = do
  cwd <- pwd
  return $ Path.resolve [cwd] p

pwd :: forall e. Sh e FilePath
pwd = _.cwd <$> State.get

cd :: forall e. FilePath -> Sh (fs :: FS, err :: EXCEPTION | e) Unit
cd = resolvePath >=> \dir -> do
  isDir <- FS.isDirectory <$> do
            liftEff $ FS.stat dir
  unless isDir do
    liftEff
      $ throwException
        $ error $ "not a directory: " ++ dir
  modify \st -> st { cwd = dir }
