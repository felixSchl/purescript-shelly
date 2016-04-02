module Test.Main where

import Prelude
import Debug.Trace
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Bind
import Shelly

main = do
  shelly do
    cd "test"
    traceShowA =<< pwd
