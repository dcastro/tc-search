module Component.BuildTypes where

import Prelude
import Data.URI as U
import Network.HTTP.Affjax as AX
import Control.Apply (lift2)
import Control.Error.Util (hush)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (search)
import DOM.HTML.Window (location)
import Data.Argonaut (decodeJson)
import Data.Array (all, filter, find, length, sortBy)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.String (toLower)
import Data.String.Utils (includes, words)
import Data.Tuple (fst, snd)
import Data.URI.Query (parseQuery)
import Global (decodeURIComponent)
import Halogen as H
import Halogen.HTML (className)
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Events.Handler (preventDefault)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Model (BuildType(..), BuildTypes(..), getName, getProject)
import Text.Parsing.StringParser (runParser)
import Text.Parsing.StringParser.Combinators (optional)
import Text.Parsing.StringParser.String (char)

type State =
  { searchText :: String
  , result :: Maybe Result
  }

type Result = Either String (Array BuildType)

type Effects eff = (ajax :: AX.AJAX, console :: CONSOLE, dom :: DOM | eff)

data Query a
  = Initialize a
  | UpdateText String a
  | NoAction a

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
    [ HH.div
        [ HP.class_ $ className "navbar-fixed"]
        [ HH.nav_
            [ HH.div
                [ HP.class_ $ className "nav-wrapper" ]
                [ HH.form
                    [ HE.onSubmit $ (\e -> preventDefault *> HE.input_ NoAction e) ]
                    [ HH.div
                        [ HP.class_ $ className "input-field" ]
                        [ HH.input
                            [ HP.id_ "search-text"
                            , HP.inputType HP.InputSearch
                            , HP.autofocus true
                            , HP.value s.searchText
                            , HE.onValueInput (HE.input UpdateText)
                            ]
                        , HH.label
                            [ HP.class_ $ className "label-icon"
                            , HP.for "search-text"
                            ]
                            [ HH.i
                                [ HP.class_ $ className "material-icons" ]
                                [ HH.text "search" ]
                            ]
                        , HH.i
                            [ HP.class_ $ className "material-icons" ]
                            [ HH.text "close" ]
                        ]
                    ]
                ]
            ]
        ]
    , HH.main
        [ HP.class_ $ className "container" ]
        [ case s.result of
            Nothing         -> loading
            Just (Left err) -> showError err
            Just (Right xs) -> renderBuildTypes $ filter (isMatch s.searchText) xs
        ]
    ]

loading :: forall p i. HTML p i
loading = HH.div
            [ HP.class_ $ className "progress" ]
            [ HH.div
                [ HP.class_ $ className "indeterminate" ]
                [ ]
            ]

showError :: forall p i. String ->  HTML p i
showError err =
  HH.div
    [ HP.classes $ className <$> [ "card-panel", "red" ] ]
    [ HH.span
        [ HP.class_ $ className "white-text" ]
        [ HH.text err ]
    ]

renderBuildTypes :: Array BuildType -> H.ComponentHTML Query
renderBuildTypes xs =
  HH.div_
    [ HH.p_ [HH.text $ "Found " <> show (length xs) <> " matches." ]
    , HH.div
        [ HP.classes $ className <$> ["collection", "z-depth-3"] ]
        (renderBuildType <$> xs)
    ]

renderBuildType :: BuildType -> H.ComponentHTML Query
renderBuildType (BuildType x) =
  HH.a
    [ HP.href x.url, HP.target "_blank", HP.class_ $ className "collection-item" ]
    [ HH.text $ x.project <> " > " <> x.name ]

eval :: forall eff. Query ~> H.ComponentDSL State Query (Aff (Effects eff))
eval (Initialize next) = do
  queryStr <- H.fromEff $ window >>= location >>= search
  next' <- maybe (pure next) (\q -> eval (UpdateText q next)) (getQuery queryStr)

  response <- H.fromAff $ attempt $ AX.get "http://localhost:8080/buildTypes"
  let result =
        response
        # bimap show _.response
        >>= decodeJson
        <#> un BuildTypes
        >>> sortBuildTypes
  H.modify (_ { result = Just result })
  pure next'
eval (UpdateText s next)  = H.modify (_ { searchText = s }) *> pure next
eval (NoAction next)      = pure next

sortBuildTypes :: Array BuildType -> Array BuildType
sortBuildTypes = sortBy (comparing getProject <> comparing getName)

fullText :: BuildType -> String
fullText = lift2 (<>) getProject getName >>> toLower

isMatch :: String -> BuildType -> Boolean
isMatch str xs = all (_ `includes` fullText xs) $ words $ toLower str

getQuery :: String -> Maybe String
getQuery queryStr = do
  U.Query (pairs) <- hush $ runParser (optional (char '?') *> parseQuery) queryStr
  pair <- find (fst >>> (_ == "q")) pairs
  value <- snd pair
  pure $ decodeURIComponent value
