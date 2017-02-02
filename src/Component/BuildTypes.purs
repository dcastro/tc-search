module Component.BuildTypes where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Network.HTTP.Affjax as AX
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Maybe (Maybe(..))
import Model (BuildType)


type State =
  { searchText :: String
  , buildTypes :: Maybe (Array BuildType)
  }

type Effects eff = (ajax :: AX.AJAX, console :: CONSOLE | eff)

data Query a = Initialize a

initialState :: State
initialState = { searchText: "", buildTypes: Nothing }

ui :: forall eff. H.Component State Query (Aff (Effects eff))
ui = H.lifecycleComponent
  { render
  , eval
  , initializer: Just $ H.action Initialize
  , finalizer: Nothing
  }

render :: State -> H.ComponentHTML Query
render s =
  HH.div_
    [
      HH.text "hello"
    ]

eval :: forall eff. Query ~> H.ComponentDSL State Query (Aff (Effects eff))
eval (Initialize next) = pure next
