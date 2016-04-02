module Test.Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Shelly

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  shelly do
    cd "foo"
