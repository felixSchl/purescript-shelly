module Shelly where

import Prelude
import Debug.Trace
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Control.Monad (when, unless)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Control.Monad.Aff.Unsafe (unsafeInterleaveAff)
import Control.Bind ((>=>), (=<<))
import Control.Monad.Aff
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.AVar (AVAR)
import Control.MonadPlus (guard)
import Control.Monad.Aff.Class (liftAff)
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
import Node.FS.Aff as FS
import Node.FS.Stats as FS
import Unsafe.Coerce
import Data.StrMap (StrMap())
import Control.Monad.Eff.Exception (Error(), EXCEPTION, throwException, error)
import Control.Monad.Error.Class (throwError, catchJust)
import Node.ChildProcess as ChildProcess
import Node.ChildProcess as Exit
import Node.ChildProcess (ChildProcess(), CHILD_PROCESS)
import Node.Stream as Stream
import Node.Buffer as Buffer
import Node.ReadLine as ReadLine
import Node.ReadLine (READLINE)
import Node.Encoding (Encoding(..))
import Data.Monoid (mempty)

foreign import code :: Error -> String

type Env = StrMap String
type State = { cwd :: String, env :: Env }
type Sh e a = StateT State (Aff e) a

-- | Evaluate the shelly action
shelly :: forall e a
        . Sh (process :: PROCESS | e) a
       -> Aff (process :: PROCESS | e) a
shelly action = do
  cwd <- liftEff Process.cwd
  env <- liftEff Process.getEnv
  evalStateT action { cwd: cwd, env: env }

-- | Launch the asyncronous shelly action
launchShelly :: forall e a
              . Sh (process :: PROCESS | e) a
             -> Eff (process :: PROCESS, err :: EXCEPTION | e) Unit
launchShelly = launchAff <<< shelly

-- | Resolve the path, relative to the cwd
resolvePath :: forall e. FilePath -> Sh e FilePath
resolvePath p = do
  cwd <- pwd
  return $ Path.resolve [cwd] p

-- | Return the current directory
pwd :: forall e. Sh e FilePath
pwd = _.cwd <$> State.get

-- | Change the current working directory
cd :: forall e. FilePath -> Sh (fs :: FS | e) Unit
cd fp = do
  dir  <- resolvePath fp
  stat <- lift do
    catchJust
      (guard <<< ("ENOENT" ==) <<< code)
      (do
        isDir <- FS.isDirectory <$> (FS.stat dir)
        unless isDir do
          throwError
            $ error
              $ "Cannot change directory."
                ++ " Not a directory: " ++ show dir)
      (\_ -> throwError
          $ error 
            $ "Cannot change directory."
              ++ " The directory does not exist: " ++ show dir)
  modify \st -> st { cwd = dir }

run :: forall e
     . String
    -> Array String
    -> Sh ( cp       :: CHILD_PROCESS
          , avar     :: AVAR
          , readline :: READLINE
          | e) Unit
run cmd args = lift do
  avar <- AVar.makeVar' []
  makeAff \reject resolve -> do

    proc <- liftEff do
      ChildProcess.spawn cmd args ChildProcess.defaultSpawnOptions


    liftEff do
      iface <- ReadLine.createInterface (ChildProcess.stdout proc)
                                        mempty

      ReadLine.setLineHandler iface \line -> do
        traceA line

    ChildProcess.onExit proc \exit -> case exit of
      Exit.Normally 0 -> resolve unit
      Exit.Normally i -> reject $ error $ "Process exited with code: " ++ show i
      Exit.BySignal s -> reject $ error $ "Process received signal: "  ++ show s
