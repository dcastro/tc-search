module Component.BuildTypes where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut (decodeJson)
import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Model (BuildType(..), BuildTypes(..))


type State =
  { searchText :: String
  , result :: Maybe Result
  }

type Result = Either String (Array BuildType)

type Effects eff = (ajax :: AX.AJAX, console :: CONSOLE | eff)

data Query a = Initialize a

initialState :: State
initialState = { searchText: "", result: Nothing }

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
      HH.p_ [HH.text "hello"]
      , case s.result of
          Nothing         -> HH.text "loading"
          Just (Left err) -> HH.text $ "Failed to load build types: " <> err
          Just (Right xs) -> renderBuildTypes xs
    ]

renderBuildTypes :: Array BuildType -> H.ComponentHTML Query
renderBuildTypes xs =
  HH.ul_ (HH.li_ <<< singleton <<< renderBuildType <$> xs)

renderBuildType :: BuildType -> H.ComponentHTML Query
renderBuildType (BuildType x) =
  HH.p_
    [ HH.text x.project
    , HH.text " > "
    , HH.a [HP.href x.url, HP.target "_blank"] [HH.text x.name]
    ]

eval :: forall eff. Query ~> H.ComponentDSL State Query (Aff (Effects eff))
eval (Initialize next) = do
  H.liftH $ liftEff $ log "initializing"
  response <- H.liftH $ liftAff $ AX.get "http://localhost:8080/buildTypes"
  let result = un BuildTypes <$> decodeJson response.response
  H.modify (_ { result = Just result })
  pure next
