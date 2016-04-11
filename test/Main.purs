module Test.Main where

import Prelude
import Debug.Trace
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Bind
import Shelly
import Node.FS (FS)
import Node.Process (PROCESS)
import Control.Monad.Eff.Exception (EXCEPTION)

main = do
  launchShelly do
    cd "test"
    run "seq" []
