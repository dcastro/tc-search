module Main where

import Prelude
import Halogen as H
import Component.BuildTypes (initialState, ui)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Halogen (runUI)
import Halogen.Util (awaitBody, runHalogenAff)
import Network.HTTP.Affjax (AJAX)

main :: Eff (H.HalogenEffects (ajax :: AJAX, console :: CONSOLE)) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui initialState body
