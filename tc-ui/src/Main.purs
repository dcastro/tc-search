module Main where

import Prelude
import Halogen as H
import Control.Monad.Eff (Eff)
import Halogen.Util (awaitBody, runHalogenAff)
import Component.BuildTypes (ui, initialState, Effects)
import Halogen (runUI)

main :: Eff (H.HalogenEffects (Effects ())) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui initialState body
