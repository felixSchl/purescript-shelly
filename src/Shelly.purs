module Shelly where

import Prelude
import Debug.Trace
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Plus (class Plus, empty)
import Control.Monad (when, unless)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Control.Monad.Aff.Unsafe (unsafeInterleaveAff)
import Control.Bind ((>=>), (=<<))
import Control.Monad.Aff
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.AVar (AVAR)
import Control.MonadPlus (class MonadPlus, guard)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Control.Monad.State.Trans (StateT(), evalStateT, modify, get)
import Control.Monad.State.Trans as State
import Control.Monad.Trans (class MonadTrans, lift)
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
import Pipes
import Pipes.Core
import Pipes.Prelude hiding (show)

foreign import code :: Error -> String

type Env = StrMap String
type State = { cwd :: String, env :: Env }
type Sh e = StateT State (Aff e)

newtype Shx e a = Shx (StateT State (Aff e) a)

runShx :: forall e a. Shx e a -> StateT State (Aff e) a
runShx (Shx x) = x

instance functorShx :: Functor (Shx e) where
  map f (Shx s) = Shx $ f <$> s

instance applyShx :: Apply (Shx e) where
  apply (Shx f) (Shx s) = Shx $ f `apply` s

instance applicativeShx :: Applicative (Shx e) where
  pure = Shx <<< pure

instance altShx :: Alt (Shx e) where
  alt (Shx x) (Shx y) = Shx $ x <|> y

instance plusShx :: Plus (Shx e) where
  empty = Shx empty

instance alternativeShx :: Alternative (Shx e)

instance bindShx :: Bind (Shx e) where
  bind (Shx x) f = Shx do
    v <- x
    runShx (f v)

instance monadShx :: Monad (Shx e)

instance monadPlusShx :: MonadPlus (Shx e)

instance monadEffShx :: MonadEff (Shx) where
  liftEff = liftEff <<< lift

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

-- run f e = do
--   v <- makeVar
--   (Right p) <- liftEff' do
--     proc <- ChildProcess.spawn "seq" ["1000000000"] ChildProcess.defaultSpawnOptions
--     iface <- ReadLine.createInterface (ChildProcess.stdout proc) mempty
--     ReadLine.setLineHandler iface \line -> do
--       launchAff $ putVar v line
--     return proc
--
--   runEffect (f (go v) $ e)
--
--   liftEff' do
--     ChildProcess.kill SIGKILL p
--
--   where
--     go v = do
--       x <- lift $ takeVar v
--       yield x
--       go v
--
--
-- main = do
--   launchAff do
--     run (\p -> p >-> (take 8000)) (\s -> do
--       lift $ log (show s)
--       return unit
--     )
--
--     log "kill it"


run
  :: forall e
   . String
  -> Array String
  -> Sh _ Int
  -- -> Producer String (Sh ( cp       :: CHILD_PROCESS
  --                        , avar     :: AVAR
  --                        , readline :: READLINE
  --                        | e)) Int
run cmd args = do
  -- liftEff do
    -- return unit
  return 1
