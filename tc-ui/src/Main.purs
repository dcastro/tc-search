module Main where

import Prelude
import Component.BuildTypes (initialState, ui)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM.HTML (window)
import DOM.HTML.Location (href)
import DOM.HTML.Window (location)
import Halogen (HalogenEffects, fromEff, runUI)
import Halogen.Util (awaitBody, runHalogenAff)
import Network.HTTP.Affjax (AJAX)

main :: Eff (HalogenEffects (ajax :: AJAX, console :: CONSOLE)) Unit
main = runHalogenAff do
  body <- awaitBody
  href <- fromEff $ window >>= location >>= href
  runUI ui (initialState href) body
