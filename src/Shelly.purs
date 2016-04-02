module Shelly where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.State.Trans (StateT(), evalStateT, modify)
import Node.Path as Path
import Node.Path (FilePath)
import Node.Process as Process
import Node.Process (PROCESS)
import Node.FS as FS
import Unsafe.Coerce

type State = { cwd :: String }
type Sh e a = StateT State (Eff e) a

runSh :: forall a e. Sh e a -> State -> Eff e a
runSh = evalStateT

shelly :: forall e a. Sh (process :: PROCESS | e) a -> Eff e a
shelly action = do
  cwd <- Process.cwd
  runSh action {  cwd: cwd }

cd :: forall e. FilePath -> Sh e Unit
cd dir = do
  modify \st -> st { cwd = dir }
