module Test.Main where

import Prelude
import Debug.Trace
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Bind
import Shelly
import Node.FS (FS)
import Node.Process (PROCESS)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe
import Control.Monad.Free.Trans
import Control.Coroutine

main = do
  launchShelly do
    cd "test"
    x <- run "seq" ["10"]
    runAff print log $ runProcess $
       x $$ consumer \s -> do
              Nothing
    cd ".."
