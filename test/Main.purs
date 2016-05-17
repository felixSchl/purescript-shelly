module Test.Main where

import Prelude
import Debug.Trace
import Data.Either
import Data.List
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Apply
import Control.Monad.Eff.Console
import Control.Bind
import Control.Lazy
import Shelly
import Node.FS (FS)
import Node.Process (PROCESS)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe
import Control.Monad.Free.Trans
import Control.Coroutine
import Control.Monad.Rec.Class

main = do
  launchShelly do
    cd "test"
    cd ".."

